# Introduction

This repository contains the code for a project where I analyzed the effect of nationality bias in the English Premier League under the guidance of my mentor, [Tony ElHabr](https://github.com/tonyelhabr). The idea of nationality bias is that players of different nationalities are rated differently; players from some nationalities tend to be overrated, while players from other nationalities tend to be underrated. More information can be found in Writeup.pdf, which was (poorly) written by my high school self but offers more information and insights. 


# Data

We used data from EA Sports FIFA and Football Reference. Data from EA Sports FIFA was downloaded [here](https://www.kaggle.com/datasets/joebeachcapital/fifa-players), while data from Football Reference was collected using the WorldFootballR package. 


# Overview of Methodology

We used shooting ratings from EA Sports FIFA and in-game data. Using in-game data and demographic data for each player (not including nationality), we performed a linear regression to predict the players’ shooting ratings in FIFA. Taking each player's predicted and actual shooting ratings, we analyzed the residuals in relation to the players’ nationalities. 


# Overview of Findings

In line with our hypothesis, we found that players from countries that are thought of as “footballing nations” (Brazil, Argentina, France, Spain, Germany, etc) tend to have overrated shooting ratings. Interestingly, players from England also tend to have underrated shooting ratings, contradicting our hypothesis. 


# Notes

I essentially learned R while doing this project, so the code is terrible and has no comments. Still, some of the analysis we did I think was really cool, albeit limited. 


# Info

Date Created: June 20 2022

