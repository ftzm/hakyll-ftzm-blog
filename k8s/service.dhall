let mkService = ./mkService.dhall

let tag = env:DOCKER_TAG as Text

in  mkService
	{ name =
		"blog"
	, port =
		80
	, tag =
		tag
	, gateway =
		"main-gateway"
	, hosts =
		[ "blog.fitzsimmons.io", "www.fitzsimmons.io", "fitzsimmons.io" ]
	}
