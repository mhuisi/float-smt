# float-smt
## Usage instructions
1. Install a version more recent than Python 3.7.4
2. Install z3-solver from pypi (https://pypi.org/project/z3-solver/)
3. Import floatsmt and get started

## Test instructions
The tests for this project are best ran individually due to the very computationally expensive validation.
They can be run e.g. like so from the float-smt root directory: `python -m unittest tests.test.Operations.test_mul`.
Note that some tests, especially the `rem`, `sqrt` and `fma` tests are unbearably slow and unlikely to complete.
The operations have largely been verified against Z3 for 16 bit floats. Notable exceptions include:
- `fma`, for which Z3 produced non-reproducible edge-cases.
- `sqrt`, which is incomplete as of now, but produces reasonable results in many cases.
- `rem`, for which we could not even complete the (5, 3) verification due to bad performance.
