project('lox', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

if get_option('with_logs')
    add_project_arguments('-DWITH_LOGS=1', language : 'c')
endif


executable('lox',
           'lex.c',
           'vm.c',
           'utils.c',
           'main.c',
           'parse.c',
           'value.c',
           'hashtab.c',
           install : true)
