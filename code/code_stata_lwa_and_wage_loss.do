

*read file (the file must contain for each observation the teleworking (tw), essentiality (e_i), and closure (c_i) scores ; scores of tw, c_i and e_i are available for 29 european countries in isco2d nace1d format, so they can be merged with any database containing that information


**** Calculation of the Lockdown Working Ability Index for each worker, based on their occupation ****


*for non-essential and non-closed (default); lwa = tw

gen lwa = tw 


*for essential occupations (e_i > 0);

replace lwa = e_i + (1-e_i)*tw if e_i > 0


*for closed occupations (c_i > 0)

replace  lwa = (1-c_i) * tw if c_i > 0



** NOW THE WAGE LOSS FOR EACH SCENARIO ONCE WE HAVE OBTAINED THE LWA 

* 2 months lockdown 
  
  gen lcklenght == 2 // we set the lockdown lenght in 2

  gen wloss2l = wage *(lcklenght/12)*(1-lwa) // wage loss
  
  gen wage2l = wage - wloss2l // final wage after the wage loss



*2 month plus 6 closure with a 20% capacity decrease

*we set the closure at 20% (activity at 80% of capacity) for 6 months
gen capacitydecr = 0.2 //capacity decrease
gen clslenght = 6 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l6c20 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l6c20 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 



*2 month plus 6 closure with a 30% capacity decrease

*we set the closure at 20% (activity at 70% of capacity) for 6 months
gen capacitydecr = 0.3 //capacity decrease
gen clslenght = 6 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l6c30 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l6c30 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 



*2 month plus 6 closure with a 40% capacity decrease

*we set the closure at 20% (activity at 60% of capacity) for 6 months
gen capacitydecr = 0.4 //capacity decrease
gen clslenght = 6 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l6c40 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l6c40 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 


** And now and alternative scenario with 9 months of partial closure 


*2 month plus 9 closure with a 20% capacity decrease

*we set the closure at 20% (activity at 80% of capacity) for 9 months
gen gen capacitydecr = 0.2 //capacity decrease
gen gen clslenght = 9 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l9c20 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l9c20 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 



*2 month plus 9 closure with a 30% capacity decrease

*we set the closure at 20% (activity at 70% of capacity) for 9 months
gen capacitydecr = 0.3 //capacity decrease
gen clslenght = 9 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l9c30 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l9c30 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 



*2 month plus 9 closure with a 40% capacity decrease

*we set the closure at 20% (activity at 90% of capacity) for 9 months
gen capacitydecr = 0.4 //capacity decrease
gen clslenght = 9 //number of months of closure

*non-closed activities (c_i = 0)
gen wage2l9c40 = wage2l if c_i == 0 //same final wage as in 2-month lockdown scenario for non-closed activities


*closed activities (c_i > 0)
gen wage2l9c40 = (wage2l - (wage * (clslenght/12) * (capacitydecr)* c_i)) if c_i > 0 
