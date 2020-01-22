ftopts = [
    '-std=c99',
    '-Wall',
    '-Wextra',
    '-Wpedantic',
    '-Wsign-conversion',
    '-Wmissing-prototypes'
]

def FlagsForFile(filename, **kwargs):
    return {'flags': ftopts}
