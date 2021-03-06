---
title: "Equations for Shiny"
author: "Damie Pak"
date: "4/11/2022"
output: html_document
---



### The Full Model 

This is a simplified model that ignores the age-terms for simplicity

$$
\frac{dS}{dt}  = \underbrace{BN(t)}_{\text{Birth}} - \underbrace{vS(t)}_{\text{Vaccination}} -\underbrace{\lambda_S S(t)}_{\text{Force of Infection}}- \underbrace{\mu S(t)}_{\text{Mortality}}
$$

$$
\frac{dS_V}{dt}  =\underbrace{vS(t)}_{\text{Vaccination}} -\underbrace{\lambda_V S_V(t)}_{\text{Force of Infection}}- \underbrace{\mu S_V(t)}_{\text{Mortality}}
$$

$$
\frac{dI_S}{dt}  = \underbrace{\lambda_S S(t)}_{\text{Force of Infection}}-\underbrace{\gamma I_S(t)}_{\text{Recovery}}- \underbrace{\mu I_S(t)}_{\text{Mortality}}
$$

$$
\frac{dI_V}{dt}  = \underbrace{\lambda_V S_V(t)}_{\text{Force of Infection}}-\underbrace{\gamma I_V(t)}_{\text{Recovery}}- \underbrace{\mu I_V(t)}_{\text{Mortality}}
$$

$$
\frac{dR}{dt}  = \underbrace{\gamma (I_S(t) +I_V(t))}_{\text{Recovery}}- \underbrace{\mu R(t)}_{\text{Mortality}}
$$

### Force of Infection

The first part of the equation ($R0 * \mu + \gamma$) is the probability of a successful disease transmission by an infected individual. The $\phi$ is a susceptibility modifier (0-1)!

The second part of the equation ($\sum\limits_{n=1}^j c_{ij} \frac{\pi(I_V) + I_S}{N}$) is the yearly contact rates between age groups $i$ and $j$ and the probability of the contact being infectious. The $pi$ is the transmissibility modifier (0-1) that can reduce the vaccinated individuals' contribution to transmission. 

$$
\lambda_S = R_0 * (\mu+\gamma) \sum\limits_{n=1}^j c_{ij} \frac{\pi(I_V) + I_S}{N}
$$


$$
\lambda_V = \phi( R_0 * (\mu+\gamma) \sum\limits_{n=1}^j c_{ij} \frac{\pi(I_V) + I_S}{N})
$$
