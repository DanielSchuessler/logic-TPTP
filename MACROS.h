#ifndef __MACROS_H

#define POINTED_FORMULA(c) \
	(Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c))))
#define POINTED_TERM(c) \
	(Pointed (Term0 (T c)) (c (Term0 (T c))))

#endif
