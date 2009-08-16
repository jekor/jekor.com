all:
	# The -k switch tells rsync to follow symbolic links to directories,
	# but not files. This is necessary to pull in the _darcs directories so
	# that they're always up-to-date but to copy other symlinks normally
	# (such as symlinks to the latest versions of files).
	rsync -avzk --delete . chris@efektiva:jekor.com/