#!/usr/bin/env python
# -*- coding: utf-8 -*-

# We are using Python 3 here
import numpy as np
import pandas
import matplotlib.pyplot as plt
import scipy
import scipy.stats
import io

FILE = '../data/train.csv'

def rental_histogram(bikedf):
    '''
    Examine the rental counts in our bike data and determine what
    distribution the data follows.  
    
    Plot two histograms on the same axes to show rentals when the
    weather is clear vs when it's inclement.
    '''

    plt.figure()
    bikedf[bikedf['weather'] == 1]['count'].hist(bins = 50, label="Clear", facecolor="yellow")
    bikedf[bikedf['weather'] > 1]['count'].hist(bins = 50, label="Inclement", facecolor="blue")
    plt.legend(title="Weather")
    plt.title(r'Bike Rentals Histogram')
    plt.xlabel("Number of Bike Rentals")
    plt.ylabel("Freqency")

    return plt

def rental_histogram_inclement(bikedf):
    '''
    Examine the rental counts in our bike data limiting it to the
    two types of inclement weather.  If these turn out to be
    related then we can use them as a single entity.
    '''

    plt.figure
    bikedf[bikedf['weather'] == 2]['count'].hist(bins = 50, label="Mist", facecolor="grey")
    bikedf[bikedf['weather'] == 3]['count'].hist(bins = 50, label="Rain", facecolor="blue")
    plt.legend(title="Inclement Weather")
    plt.title(r'Bike Rentals (Inclement) Histogram')
    plt.xlabel("Number of Bike Rentals")
    plt.ylabel("Frequency")

    return plt

def mann_whitney_plus_means(v1, v2):
    '''
    Takes the means and runs a Mann Witney U-test on the 'count' column.

    Returns:
      * mean of entries when weather is clear
      * mean of entries when for inclement weather
      * Mann-Whitney U-Statistic and p-value comparing the number of rentals
      when it's clear vs when it's not
    '''
    (U, p) = scipy.stats.mannwhitneyu(v1, v2)

    return np.mean(v1), np.mean(v2), U, p * 2

def explore(training_file):
    df = pandas.read_csv(training_file)

    print('''
    QUESTION
    ========
    Can we combine any of our weather types into a single coefficient for our final
    algorithm?
    ''')
    input("\nPress Enter to continue...")

    print("\nThe histogram for the two sets are both right-skewed so we'll be using\n"
          "a Mann-Whitney U test as opposed to a t-test for our results")

    plt = rental_histogram(df)
    plt.show()

    clear_mean, inclement_mean, U, p = mann_whitney_plus_means(df[df['weather'] == 1]['count'],
                                                               df[df['weather'] > 1]['count'])
    print("-"*28)
    print("\n Clear vs Inclement Weather")
    print("-"*28)
    print("\nMann Whitney Results:\n\tClear Mean:", clear_mean,"\n\tInclement Mean:", inclement_mean,
            "\n\tMann Whitney U:", U, "\n\tp-value:", p,"\n")

    # Conclusions
    if (p < 0.05):
        print("Since the p-value is less than 0.05 we can conclude that there is a\n"
              "difference between the distributions.  It is statistically significant\n"
              "enough that we can conclude that when the weather is clear it affects\n"
              "the number of riders.")
        if (clear_mean > inclement_mean):
            print("\nFurther to that since the mean number of riders on a clear day\n"
                  "is greater than the inclement days, we can conclude that more\n"
                  "riders go out when the days are clear. (which may be obvious)")
        elif (clear_mean < inclement_mean):
            print("\nStrangely more riders go out when the weather is bad.\n")
        else:
            print("\n...and yet the means are the same no matter what the weather...\n")
    else:
        print("Since the p-value is greater than 0.05 we can conclude that there is no\n"
              "statiscal difference between the number of riders no matter what the\n"
              "weather is\n")

    print("\nThere is only one entry for severe weather so the next step is to see if\n"
          "the last two types of inclement weather could have been derived from the same\n"
          "population\n")

    plt = rental_histogram_inclement(df)
    plt.show()

    mist_mean, rain_mean, U, p = mann_whitney_plus_means(df[df['weather'] == 2]['count'],
                                                               df[df['weather'] == 3]['count'])

    print("-"*26)
    print("  Misty vs Rainy Weather")
    print("-"*26)
    print("\nMann Whitney Results:\n\tMist Mean:", mist_mean,"\n\tRain Mean:", rain_mean,
            "\n\tMann Whitney U:", U, "\n\tp-value:", p,"\n")

    print('''
    ==========
    CONCLUSION
    ==========
    With such a small p-value we have to conclude that we need to treat each of our 
    weather types as separate coefficients in our final algorithm.
    ''')

if __name__ == "__main__":
    explore(FILE)
