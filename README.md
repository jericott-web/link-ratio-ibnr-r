
This project implements Link Ratio methods for IBNR reserving based on the *Claims Reserving Manual*, Volume 1, Chapter E (Pages 161-182), 1997 Edition, published by the Faculty and Institute of Actuaries.

The repository contains R implementations of four key reserving techniques from the Manual:

1. **Worst Case Method (Maximum Link Ratios)** - Uses the maximum observed development factor for each period (most conservative approach). Reference: Section E5.

2. **Average Method** - Applies simple averages of observed link ratios. Reference: Section E6.

3. **Chain Ladder (Original Weights)** - Volume-weighted development factors method. Reference: Section E8.

4. **Trending Method** - Projects future link ratios using least squares trend fitting. Reference: Section E9.

Note: The results produced by this implementation may differ slightly from those shown in the Claims Reserving Manual. 
This is due to the fact that the manual applies rounded values in intermediate steps, while this R implementation uses full numeric precision throughout. 
However, the starting triangle used is exactly the same as in the manual (Section E5.2), ensuring methodological fidelity.
