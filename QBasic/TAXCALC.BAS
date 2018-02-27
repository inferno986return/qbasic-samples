10 'This program will calculate the total cost of a product
20 'after you tell it the original cost and the sales tax rate.
30 '
40 'It was written by me, Bryan Schneider, and I toiled over it for many
50 'hours, but out of the goodness of my heart, I am distributing
60 'this valuable program as FREEWARE!
70 '
80 '
90 '
100 GOSUB 1000          'Input Date
200 GOSUB 2000          'Calculations
300 GOSUB 3000          'Output
999 END                 'End of Program
1000 '*******************************************************************
1002 ' INPUT DATA
1004 '*******************************************************************
1005 PRINT
1006 PRINT "Hi there! I'm going to tell you the true cost of that little";
1007 PRINT "item you've got your heart set on."
1008 PRINT
1010 INPUT "... and your name is"; nam$
1020 INPUT "... and you're planning to buy a"; product$
1030 PRINT "... and they are going to sock you how many"
1040 INPUT "... dollars for it (no commas or $)"; Price
1050 PRINT "... and your sales tax is how absurd?"
1060 PRINT "... Well, I mean, how would you describe it"
1080 INPUT "... as a decimal (.05 = 5%)"; Rate
1090 RETURN
1999 '
2000 '*********************************************************************
2010 ' CALCULATIONS
2020 '*********************************************************************
2030 Tax = Price * Rate         'Calculates the amount of sales tax
2040 Total = Price + Tax        'Calculates the total cost including sales tax
2110 PRINT
2060 RETURN
2999 '
3000 '*******************************************************************
3010 ' OUTPUT
3020 '*******************************************************************
3025 PRINT
3030 PRINT "Well, "; nam$; ", it's like this see?"
3035 PRINT "The government needs more money to waste,"
3040 PRINT "so your $"; Price; product$; " is really going to"
3045 PRINT "set you back "; : PRINT USING "$###.##"; Total
3050 PRINT "Sorry about that!"
3055 PRINT
3060 RETURN
3997 '*******************************************************************
3998 ' THAT'S ALL FOLKS!
3999 '*******************************************************************

