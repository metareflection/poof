all: test

test:
	scheme 00-all-poof.scm

.DUMMY: all test
