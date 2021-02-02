all: test

test:
	scheme 00-all-poof.scm 99-exit.scm

.DUMMY: all test
