use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    'NAME'		=>	'Util',
    'VERSION_FROM'	=>	'Util.pm', 	# finds $VERSION
    'PREREQ_PM'		=>	{}, 		# e.g. Module::Name => 1.1
    'LIBS'		=>	[''], 		# e.g. '-lm'
    # 'DEFINE'		=>	'-DUTIL_CLONE_DEBUG -DUTIL_DEBUG',# e.g. '-DHAVE_SOMETHING'
    # 'OPTIMIZE'	=>	'-g',
    # 'OPTIMIZE'	=>	'-pg',
    'OPTIMIZE'	=>	'-O3',
    'INC'		=>	'' 		# e.g. '-I/usr/include/other'
);
