# Ashfall-AirportDisruption

Code for creating fragility functions using airport closure duration and ashfall thickness data. 
For more details, please refer to: 

Lerner, G. A., Teng, N. R. X., Jenkins, S. F., Lallemant, D., Tupper, A., Hayes, J. L., Williams, G. T., Joffrain, M., Wardman, J., & Lira-Beltrán, R. M. (2026). Impacts-based analysis of disruption to airport operations by volcanic ashfall. *J Appl. Volcanol*, *15*, 7. https://doi.org/10.1186/s13617-026-00164-9

<br>

**General workflow to generate fragility functions using the script**

1. Prepare data for fitting - requires 1) hazard intensity metric and 2) damage states
- Select hazard intensity metric (HIM) e.g. ashfall thickness
- Assign impact to impact states (IS) e.g. <1 day -> IS1; 1-2 days -> IS2

<br> 

2. Fit data using cumulative link model (CLM)
- Specify relationship between IS and HIM e.g. IS ~ log(HIM)

<br>

3. Calculate probabilities of equalling or exceeding each IS for a range of continuous HIM
- Generate continuous HIM values
- Use predict function to obtain linear predictor for continuous HIM values
- Use the appropriate inverse link function to obtain probabilities
