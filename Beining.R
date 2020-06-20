payments_schedule = function(annual_interest_rate, principal, years) {
  #first calculate monthly interest rate 
  #second calculate the constant monthly payment amount
  monthly_interest_rate = annual_interest_rate / 12
  payment_months = years * 12
  a = (1 + monthly_interest_rate) ^ payment_months - 1
  total_payment = principal * monthly_interest_rate * (a + 1) / a
  
  interest_payment = principal_payment = unpaid_balance = total_payments = vector("numeric", payment_months)
  upb = principal 
  for (i in 1:payment_months) {
    intrst = upb * monthly_interest_rate
    pricil = total_payment - intrst
    upb = upb - pricil
    
    
    interest_payment[i] = intrst
    principal_payment[i] = pricil
    unpaid_balance[i] = upb
    total_payments[i] = total_payment
  }
  
  knitr::kable(data.frame(month = 1:payment_months, interest_payment, principal_payment, total_payments, unpaid_balance),
               caption = "Payment schedule")
}

payments_schedule(0.03, 100000, 30)





