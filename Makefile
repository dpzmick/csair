PKG=atdgen

all: test.native

test.native: test.ml map_data_parser
	corebuild -pkg $(PKG) $@

clean_test:
	-@rm test.native
	-@rm -rf _build

map_data_parser: map_data.atd
	atdgen -j $^
	atdgen -t $^

clean_map_data_parser:
	-@rm map_data_j.* map_data_t.*

clean: clean_test clean_map_data_parser
