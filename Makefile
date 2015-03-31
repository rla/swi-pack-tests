all: testpacks

PACKS = testpack/standalone-0.0.1.tgz testpack/tampered-0.0.1.tgz \
	testpack/dependant-0.0.1.tgz testpack/foreign-0.0.1.tgz \
	testpack/from-github/v0.0.1.zip

testpacks: $(PACKS)

testpack/standalone-0.0.1.tgz: testpack/standalone-0.0.1/*.pl
	tar -C testpack/standalone-0.0.1 -czvf $@ .

testpack/tampered-0.0.1.tgz: testpack/tampered-0.0.1/*.pl
	tar -C testpack/tampered-0.0.1 -czvf $@ .

testpack/dependant-0.0.1.tgz: testpack/dependant-0.0.1/*.pl
	tar -C testpack/dependant-0.0.1 -czvf $@ .

testpack/foreign-0.0.1.tgz: testpack/foreign-0.0.1/* testpack/foreign-0.0.1/raw/*
	tar -C testpack/foreign-0.0.1 -czvf $@ .

testpack/from-github/v0.0.1.zip: testpack/from-github/v0.0.1/* testpack/from-github/v0.0.1/prolog/*
	cd testpack/from-github/v0.0.1 && zip -r ../v0.0.1.zip .

clean:
	rm -f testpack/*.tgz
	rm -f testpack/from-github/*.zip

.PHONY: all testpacks clean
