# Infectious-Disease-Modelling-in-R
Projecting the Spread of COVID-19 at Culver Academies with deterministic and stochastic models.
Culver Academies, a college preparatory boarding school located in Culver, Indiana has been making
effort to mitigate the spread of COVID-19 introduced by an index case on November 1st, 2020 among the
student body.
This project builds a complex deterministic compartmental model to project the spread with Euler's
Method. Then, the project constructs a stochastic reproductive number simulator with random variables
in R Studio. To calculate model parameters, 30 students from three wellness classes were recruited to
investigate their daily contact habits. Parameter fitting also uses statistical analysis on Secondary Attack
Rates (SAR) of COVID-19 by contact scenarios through collection of about 70 data points from published
scholarly articles.
The deterministic model projected accurately that maximum incidence arrives during the second week
after introduction of index case but significantly overestimate prevalence, incidence, and cumulative
incidence. Tactical pause (a policy adopted by Culver Academies in November), one extra viral test per
test session, and contact tracing are recognized as effective interventions at spread mitigation while
enforced dorm/barracks mask mandate and one extra viral test session per week are less effective.
These findings shed light on the selection of intervention policies Culver Academies faces when
confronted with spread of COVID-19 in the student body.

METHODOLOGY
