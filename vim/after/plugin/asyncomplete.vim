au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#flow#get_source_options({
	\ 'name': 'flow',
	\ 'whitelist': ['javascript'],
	\ 'completor': function('asyncomplete#sources#flow#completor'),
	\ 'config': {
	\ 	'prefer_local': 1,
	\ 	'show_typeinfo': 1
	\ },
	\ }))

au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
	\ 'name': 'file',
	\ 'whitelist': ['*'],
	\ 'priority': 10,
	\ 'completor': function('asyncomplete#sources#file#completor')
	\ }))

au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#gocode#get_source_options({
	\ 'name': 'gocode',
	\ 'whitelist': ['go'],
	\ 'completor': function('asyncomplete#sources#gocode#completor'),
	\ }))
