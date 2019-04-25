# arising-matters

This explores wether the Rinne et al paper is problematic when applied to real turbine data. What I did is the following: I calculated total cost of a turbine (i.e. the specific cost as given by rinne times capacity: power * (620 * log(hh) - power/(r^2 * pi)+182 * sqrt(age)-1005)).

I then took the first derivative and set it to 0 to find, where the maximum cost of the function is with respect to the power of the turbine: max_power <- (r^2*pi)*(620*log(hh) + 182*sqrt(age) - 1005) / (2*1.68).

Afterwards I plotted max_power, changing r (the radius). I did this for different hub_heights and plotted the results.

Additionally, I downloaded real turbine data (i.e. power and rotor length) and also plotted it in the figure. Points which are under the lines indicate that they are somehow ok. Points above the line indicate that this turbine is cheaper than a smaller turbine (in terms of power) when applying the Rinne equation. This should definitely not be the case!

This is the result:

![alt text](../figures/max_power_rinne_vs_real_turbines.png)


  
