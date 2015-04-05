######## logistic regression model 


#### step-wise feature selection
## model for casual ridership
lm_cas_step <- glm(casual ~ . - datetime - casual - registered - count - dateIndex - date, data=train, family='poisson')
lm_cas_step <- step(lm_cas_step, direction='both')
summary(lm_cas_step)

## model for registered ridership
lm_reg_step <- glm(registered ~ . - datetime - casual - registered - count - dateIndex - date, data=train, family='poisson')
lm_reg_step <- step(lm_reg_step, direction='both')
summary(lm_reg_step)

## model for total ridership
lm_tot_step <- glm(count ~ . - datetime - casual - registered - count - dateIndex - date, data=train, family='poisson')
lm_tot_step <- step(lm_tot_step, direction='both')
summary(lm_tot_step)


#### manual feature selection
## model for casual ridership
lm_cas_manual <- glm(casual ~ workingday + weather + 
                      temp + humidity + windspeed + 
                      year + month + hour + 
                      w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                      workingday * hour, 
                    data=train, family='poisson')
summary(lm_cas_manual)

## model for registered ridership
lm_reg_manual <- glm(registered ~ workingday + weather + 
                      temp + humidity + windspeed + 
                      year + month + hour + 
                      w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                      workingday * hour, 
                    data=train, family='poisson')
summary(lm_reg_manual)

## model for total ridership
lm_tot_manual <- glm(count ~ workingday + weather + 
                      temp + humidity + windspeed + 
                      year + month + hour + 
                      w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                      workingday * hour,
                    data=train, family='poisson')
summary(lm_tot_manual)



#### 3rd attempt: manual + feature selection combined
## model for casual ridership
lm_cas_3 <- glm(casual ~ workingday + holiday + day +
                 weather + season +
                 temp + humidity + windspeed + 
                 year + month + hour + 
                 w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                 workingday * hour + 
                 workingday * w40 + 
                 workingday * t35 + 
                 workingday * davg_w20 + 
                 workingday * davg_t30 + 
                 workingday * davg_h75,
               data=train, family='poisson')
lm_cas_3 <- step(lm_cas_3, direction='both')
summary(lm_cas_3)

## model for registered ridership
lm_reg_3 <- glm(registered ~ workingday + holiday + day +
                 weather + season +
                 temp + humidity + windspeed + 
                 year + month + hour +                       
                 w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                 workingday * hour + 
                 workingday * w40 + 
                 workingday * t35 + 
                 workingday * davg_w20 + 
                 workingday * davg_t30 + 
                 workingday * davg_h75,
               data=train, family='poisson')
lm_reg_3 <- step(lm_reg_3, direction='both')
summary(lm_reg_3)

## model for total ridership
lm_tot_3 <- glm(count ~ workingday + holiday + day + 
                 weather + season +
                 temp + humidity + windspeed + 
                 year + month + hour +                       
                 w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
                 workingday * hour + 
                 workingday * w40 + 
                 workingday * t35 + 
                 workingday * davg_w20 + 
                 workingday * davg_t30 + 
                 workingday * davg_h75,
               data=train, family='poisson')
lm_tot_3 <- step(lm_tot_3, direction='both')
summary(lm_tot_3)



#### prediction with manual model
pred_cas_manual <- round(predict(lm_cas_manual, newdata=cv, type='response'))
pred_reg_manual <- round(predict(lm_reg_manual, newdata=cv, type='response'))
pred_tot_manual <- round(predict(lm_tot_manual, newdata=cv, type='response'))
pred_comb_manual <- pred_cas_manual + pred_reg_manual

calcRMSLE(pred_cas_manual, cv$casual)
calcRMSLE(pred_reg_manual, cv$registered)
calcRMSLE(pred_tot_manual, cv$count)
calcRMSLE(pred_comb_manual, cv$count)



#### prediction with step model
pred_cas_step <- round(predict(lm_cas_step, newdata=cv, type='response'))
pred_reg_step <- round(predict(lm_reg_step, newdata=cv, type='response'))
pred_tot_step <- round(predict(lm_tot_step, newdata=cv, type='response'))
pred_comb_step <- pred_cas_step + pred_reg_step

calcRMSLE(pred_cas_step, cv$casual)
calcRMSLE(pred_reg_step, cv$registered)
calcRMSLE(pred_tot_step, cv$count)
calcRMSLE(pred_comb_step, cv$count)



#### prediction with model3
pred_cas_3 <- round(predict(lm_cas_3, newdata=cv, type='response'))
pred_reg_3 <- round(predict(lm_reg_3, newdata=cv, type='response'))
pred_tot_3 <- round(predict(lm_tot_3, newdata=cv, type='response'))
pred_comb_3 <- pred_cas_3 + pred_reg_3

calcRMSLE(pred_cas_3, cv$casual)
calcRMSLE(pred_reg_3, cv$registered)
calcRMSLE(pred_tot_3, cv$count)
calcRMSLE(pred_comb_3, cv$count)

