Set of tests for SWI-Prolog packs. Contains fake testing packs. Pack rebuilding breaks tests
as tar is not deterministic. Assume that https://github.com/SWI-Prolog/swipl-devel/pull/43 is applied.

Running:

```
swipl -s tests.pl
?- run_tests.
```

Tests write lots of output into stderr.