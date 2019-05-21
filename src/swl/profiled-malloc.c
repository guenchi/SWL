/*
 * malloc.c (Caltech) 2/21/82
 * Chris Kingsley, kingsley@cit-20.
 *
 *----------------------------------------------------------------------------
 * modified by John Zuckerman, Motorola GEG, 1990.
 *
 * modified by John Zuckerman, Motorola GEG, 13 July 1992;
 * now allocates in multiples of 16K chunks.
 *
 * modified by John Zuckerman, Motorola GEG, 18 Aug 1992;
 * F_ASSERT (formerly ASSERT) now prints a more accurate error
 * message after a botch.  Also, it no longer terminates the
 * program, as this may be unnecessary in some cases.
 *
 * modified by John Zuckerman, Motorola GEG, 15 Feb 1993;
 * F_ASSERT was broken in ANSI-C. Replaced with manually expanded
 * code.
 *
 * modified by John Zuckerman, Motorola GEG, 01 Apr 1993;
 * enhanced the malloc monitor code.
 *
 *----------------------------------------------------------------------------
 * This is a very fast storage allocator.  It allocates blocks of a small 
 * number of different sizes, and keeps free lists of each size.  Blocks that
 * don't exactly fit are passed up to the next larger size.  In this 
 * implementation, the available sizes are 2^n-4 (or 2^n-12) bytes long.
 * This is designed for use in a program that uses vast quantities of memory,
 * but bombs when it runs out. 
 */

#include <stdio.h>

static findbucket(), morecore();

typedef char * caddr_t;
#define u_char unsigned char
#define u_int unsigned int
#define u_short unsigned short

/*
 * The overhead on a block is at least 4 bytes.  When free, this space
 * contains a pointer to the next free block, and the bottom two bits must
 * be zero.  When in use, the first byte is set to MAGIC, and the second
 * byte is the size index.  The remaining bytes are for alignment.
 * If range checking is enabled and the size of the block fits
 * in two bytes, then the top two bytes hold the size of the requested block
 * plus the range checking words, and the header word MINUS ONE.
 */
union	overhead {
	union	overhead *ov_next;	/* when free */
#if defined (mips) || defined (sparc)
	double  strut;			/* alignment problems */
#endif
	struct {
		u_char	ovu_magic;	/* magic number */
		u_char	ovu_index;	/* bucket # */

#ifdef SAFETY_CHECKS
		u_short	ovu_size;	/* actual block size */
		u_int	ovu_rmagic;	/* range magic number */
#endif

#ifdef MALLOC_MONITOR
		u_int  realbytes;
#endif
	} ovu;
#define	ov_magic	ovu.ovu_magic
#define	ov_index	ovu.ovu_index
#define	ov_size		ovu.ovu_size
#define	ov_rmagic	ovu.ovu_rmagic
};

#define	MAGIC		0xff		/* magic # on accounting info */
#define RMAGIC		0x55555555	/* magic # on range info */
#ifdef SAFETY_CHECKS
#define	RSLOP		sizeof (u_int)
#else
#define	RSLOP		0
#endif

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The overhead information
 * precedes the data area returned to the user.
 */
#define	NBUCKETS 20
static	union overhead *nextf[NBUCKETS];
extern	char *sbrk();

#ifdef MALLOC_STATS
/*
 * nmalloc[i] is the number of currently malloc'd blocks for a given 
 * block size.
 *
 * tmalloc[i] is total number of malloc'd blocks for a given block size
 * since system startup.
 */
static	u_int tmalloc[NBUCKETS];
static	u_int nmalloc[NBUCKETS];
#endif

#ifdef SAFETY_CHECKS
static
fbotch(s)
     char *s;
{
  fprintf(stderr, "free: assertion botched: %s\n", s);
  fprintf(stderr, "The system may be corrupted.\n");
}
#endif

char *
malloc(nbytes)
     register unsigned nbytes;
{
  register union overhead *p;
  register int bucket = 0;
  register unsigned shiftr;


#ifdef MALLOC_MONITOR
  int realbytes = nbytes;

  fprintf(stderr,"     malloc(%d)\n",nbytes);
#endif

  /*
   * Convert amount of memory requested into
   * closest block size stored in hash buckets
   * which satisfies request.  Account for
   * space used per block for accounting.
   */
  nbytes += sizeof (union overhead) + RSLOP;
  nbytes = (nbytes + 3) &~ 3; 
  shiftr = (nbytes - 1) >> 2;
  /* apart from this loop, this is O(1) */
  while (shiftr >>= 1)
    bucket++;
  /*
   * If nothing in hash bucket right now,
   * request more memory from the system.
   */
  if (nextf[bucket] == NULL)    
    morecore(bucket);
  if ((p = (union overhead *)nextf[bucket]) == NULL)
    return (NULL);
  /* remove from linked list */

#ifdef SAFETY_CHECKS
  if (*((int*)p) & (sizeof(union overhead) - 1))
    fprintf(stderr,"malloc: corrupt malloc ptr 0x%x at 0x%x\n",*((int*)p),p);
#endif

  nextf[bucket] = p->ov_next;
  p->ov_magic = MAGIC;
  p->ov_index= bucket;

#ifdef MALLOC_STATS
  nmalloc[bucket]++;
  tmalloc[bucket]++;
#endif

#ifdef SAFETY_CHECKS
  /*
   * Record allocated size of block and bound space with magic numbers.
   */
  if (nbytes <= 0x10000)
    p->ov_size = nbytes - 1;
  p->ov_rmagic = RMAGIC;
  *((u_int *)((caddr_t)p + nbytes - RSLOP)) = RMAGIC;
#endif

#ifdef MALLOC_MONITOR
  p->realbytes = realbytes;
#endif

  return ((char *)(p + 1));
}

/*
 * Allocate more memory to the indicated bucket.
 */
static
morecore(bucket)
	register int bucket;
{
  	register union overhead *op;
  	register int rnu;       /* 2^rnu bytes will be requested */
  	register int nblks;     /* become nblks blocks of the desired size */
	register int siz;

  	if (nextf[bucket])
  		return;
	/*
	 * Insure memory is allocated
	 * on a page boundary.  Should
	 * make getpageize call?
	 */

#if 0  /* johnz */
  	op = (union overhead *)sbrk(0);
#ifndef I286
  	if ((int)op & 0x3ff)
  		(void)sbrk(1024 - ((int)op & 0x3ff));
#else
	/* The sbrk(0) call on the I286 always returns the next segment */
#endif
#endif 

/* take 16K bytes minimum - johnz */

  	rnu = (bucket <= 11) ? 14 : bucket + 3;

  	nblks = 1 << (rnu - (bucket + 3));  /* how many blocks to get */
  	if (rnu < bucket)
		rnu = bucket;

#ifdef MALLOC_MONITOR
    fprintf(stderr,"         malloc: sbrk request: %d\n",1 << rnu);
#endif

	op = (union overhead *)sbrk(1 << rnu);
	/* no more room! */
  	if ((int)op == -1)
  		return;
	/*
	 * Round up to minimum allocation size boundary
	 * and deduct from block count to reflect.
	 */
#ifndef I286
  	if ((int)op & 7) {
  		op = (union overhead *)(((int)op + 8) &~ 7);
  		nblks--;
  	}
#else
	/* Again, this should always be ok on an 80286 */
#endif
	/*
	 * Add new memory allocated to that on
	 * free list for this hash bucket.
	 */
  	nextf[bucket] = op;
  	siz = 1 << (bucket + 3);
  	while (--nblks > 0) {
		op->ov_next = (union overhead *)((caddr_t)op + siz);
		op = (union overhead *)((caddr_t)op + siz);
  	}
}

free(cp)
     char *cp;
{   
  register int size;
  register union overhead *op;

  if (cp == NULL)
    return;
  op = (union overhead *)((caddr_t)cp - sizeof (union overhead));

#ifdef SAFETY_CHECKS
  if (!(op->ov_magic == MAGIC)) {
    fbotch("op->ov_magic == MAGIC");
    return;
  }
  if (!(op->ov_index < NBUCKETS)) {
    fbotch("op->ov_index < NBUCKETS");
    return;
  } 
  if (!(op->ov_rmagic == RMAGIC)) {
    fbotch("Beginning of block has been overrun");
    return;
  }
  if (op->ov_index <= 13)
    if (!(*(u_int *)((caddr_t)op + op->ov_size + 1 - RSLOP) == RMAGIC)) {
      fbotch("End of block has been overrun");
      return;
    }
#endif

  size = op->ov_index;
  op->ov_next = nextf[size];
  nextf[size] = op;

#ifdef MALLOC_STATS
  nmalloc[size]--;
#endif


#ifdef MALLOC_MONITOR
  fprintf(stderr,"           free(%d) size=%d\n", cp, op->realbytes);
#endif
}

/*
 * When a program attempts "storage compaction" as mentioned in the
 * old malloc man page, it realloc's an already freed block.  Usually
 * this is the last block it freed; occasionally it might be farther
 * back.  We have to search all the free lists for the block in order
 * to determine its bucket: 1st we make one pass thru the lists
 * checking only the first block in each; if that fails we search
 * ``reall_srchlen'' blocks in each list for a match (the variable
 * is extern so the caller can modify it).  If that fails we just copy
 * however many bytes was given to realloc() and hope it's not huge.
 */
int reall_srchlen = 4;	/* 4 should be plenty, -1 =>'s whole list */

char *
realloc(cp, nbytes)
	char *cp; 
	unsigned nbytes;
{   
  	register u_int onb;
	union overhead *op;
  	char *res;
	register int i;
	int was_alloced = 0;

  	if (cp == NULL)
  		return (malloc(nbytes));
	op = (union overhead *)((caddr_t)cp - sizeof (union overhead));
	if (op->ov_magic == MAGIC) {
		was_alloced++;
		i = op->ov_index;
	} else {
		/*
		 * Already free, doing "compaction".
		 *
		 * Search for the old block of memory on the
		 * free list.  First, check the most common
		 * case (last element free'd), then (this failing)
		 * the last ``reall_srchlen'' items free'd.
		 * If all lookups fail, then assume the size of
		 * the memory block being realloc'd is the
		 * smallest possible.
		 */
		if ((i = findbucket(op, 1)) < 0 &&
		    (i = findbucket(op, reall_srchlen)) < 0)
			i = 0;
	}
	onb = (1 << (i + 3)) - sizeof (*op) - RSLOP;
	/* avoid the copy if same size block */
	if (was_alloced &&
	    nbytes <= onb && nbytes > (onb >> 1) - sizeof(*op) - RSLOP) {
#ifdef SAFETY_CHECKS
		/*
		 * Record new allocated size of block and
		 * bound space with magic numbers.
		 */
		if (op->ov_index <= 13) {
			/*
			 * Convert amount of memory requested into
			 * closest block size stored in hash buckets
			 * which satisfies request.  Account for
			 * space used per block for accounting.
			 */
			nbytes += sizeof (union overhead) + RSLOP;
			nbytes = (nbytes + 3) &~ 3; 
			op->ov_size = nbytes - 1;
			*((u_int *)((caddr_t)op + nbytes - RSLOP)) = RMAGIC;
		}
#endif
		return(cp);
	}
  	if ((res = malloc(nbytes)) == NULL)
  		return (NULL);
  	if (cp != res)			/* common optimization */
		(void)bcopy(cp, res, (int)((nbytes < onb) ? nbytes : onb));
  	if (was_alloced)
		free(cp);
  	return (res);
}

/*
 * Search ``srchlen'' elements of each free list for a block whose
 * header starts at ``freep''.  If srchlen is -1 search the whole list.
 * Return bucket number, or -1 if not found.
 */
static
findbucket(freep, srchlen)
	union overhead *freep;
	int srchlen;
{
	register union overhead *p;
	register int i, j;

	for (i = 0; i < NBUCKETS; i++) {
		j = 0;
		for (p = nextf[i]; p && j != srchlen; p = p->ov_next) {
			if (p == freep)
				return (i);
			j++;
		}
	}
	return (-1);
}

#ifdef MALLOC_STATS
/*
 * xfMstats - print out statistics about malloc
 * 
 * Prints two columns of numbers, one showing the length of the free list
 * for each size category, the second showing the number of mallocs
 * for each size category.
 */
xfMstats()
{
  register int i, j;
  register union overhead *p;
  int totfree = 0,
  totused = 0;

  fprintf(stderr, "Memory allocation statistics\n");
    fprintf(stderr, "%12s        %9s %9s %9s\n","size",
	    "free","used","mallocs");

  for (i = 0; i < NBUCKETS; i++) {
    for (j = 0, p = nextf[i]; p; p = p->ov_next, j++);
    fprintf(stderr, "%12d(2^%2d) %9d %9d %9d\n", 
	    (1 << (i + 3)), 
	    i + 3,
	    j, 
	    nmalloc[i],
	    tmalloc[i]);
    totfree += j * (1 << (i + 3));
    totused += nmalloc[i] * (1 << (i + 3));
  }
  fprintf(stderr, "\n\tTotal bytes in use: %d, total free: %d\n",
	  totused, totfree);
}
#endif
/*========================== end malloc.c ==============================*/
