test:
	$IDRIS_PACK_HOME/bin/spec-discover test
	pack test extrafun

build:
	pack build
