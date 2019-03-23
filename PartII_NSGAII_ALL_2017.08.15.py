## Adapted "inspyred_NSGAII_adaptedthread.py"
## Link CFA parcel sets with algorithm

# call modules
import inspyred 
import arcpy
import glob, os, random
from time import time, ctime

# setup arcpy
arcpy.CheckOutExtension("Spatial")
arcpy.env.workspace = r"P:\GeneticAlgorithm\GADerivedData\CFA_Parcel_Shapefiles"
arcpy.env.overwriteOutput = 1



########################################################



# create lists to hold CFA names and names of CFA shapefiles
cfaLst = []
shpLst = []
os.chdir(r"P:\GeneticAlgorithm\GADerivedData\CFA_Parcel_Shapefiles")
print "\n>> These CFA's have shapefiles available:"
for shpfile in glob.glob("*_parcels_zones_MINobj.shp"): 
    name = shpfile.split("_")[0]
    cfaLst.append(name)
    print name
    
    shpLst.append(shpfile)



########################################################



# export parcel attributes to dictionary so don't have to keep rerunning cursor
cfaDict = {}
    
# generate parcel attribute lists for each CFA shapefile
for shp in shpLst:
    
    # get CFA name from file name
    name = str(shp.split("_")[0])
    #print "\n> Current focus area = %s"%name

    # identify field names from the cfa shapefile
    fieldLst = arcpy.ListFields(shp)
    fieldName = [f.name for f in fieldLst]

    # add dictionary entry named for the CFA
    cfaDict['{0}'.format(name)] = []

    # create cursor for the cfa shapefile
    rows = arcpy.SearchCursor(shp)

    # loop through rows to extract variables of interest
    for row in rows:
        
        newLine = ""
        
        # OBJECTIVE = Connectivity -> minimize distance between parcels
        # extract unique identifiers for parcels to use in spatial analysis
        if "NPARNO" in fieldName:
            uniqueID = str(row.getValue("NPARNO"))
        if "NHGIS_ID" in fieldName:
            uniqueID = str(row.getValue("NHGIS_ID"))
        if "LOC_ID" in fieldName:
            uniqueID = str(row.getValue("LOC_ID"))

        # OBJECTIVE = Parcel size -> maximize total area
        if "LOT_SIZE" in fieldName:
            Area = round(float(row.getValue("LOT_SIZE")), 0)
        if "SIZE" in fieldName:
            Area = round(float(row.getValue("SIZE")), 0)

        # OBJECTIVE = Parcel cost -> minimze total cost
        Cost = round(row.getValue("TotVALmill"), 6)
        
        # OBJECTIVE = Priority habitat -> maximize Forest Upland and Wetland
        #CntHardSwamp = float(row.getValue("CentralHar")) 
        #NoHardConif = float(row.getValue("NorthHardw")) 
        nonForest = float(row.getValue("nonForest")) 

        # OBJECTIVE = Areas with development risk -> maximize development probability
        ProbDevel = round(row.getValue("MEAN"), 4)

        # OBJECTIVE = Alignment with town character -> minimize weight-by-area score (avoid town priority areas)
        TownChar = round(row.getValue("WghtByArea"), 2)
        
        # pull out attributes of interest
        parcelAttributes = [uniqueID, Area, Cost, nonForest, ProbDevel, TownChar] 

        # add them to the dictionary
        cfaDict[name].append(parcelAttributes)

    # close cursor
    del row, rows

# number of parcels in each CFA
print "\n>> Number of parcels in each CFA:"
parcCntLst = []
for cfa in cfaLst:
    parcCnt = len(cfaDict[cfa])
    parcCntLst.append(parcCnt)
    print "{0} = {1} parcles".format(cfa,parcCnt)



########################################################


  
# Define parts of the dictionary

print "EXPLANATION OF THE DICTIONARY STRUCTURE"

print "\n> Dictionary name = cfaDict"
print "> Dictionary organization: \ncfaDict['CFA name'][List of attributes for each parcel]"

print "> Order of attributes for each parcel:"
print "FiD, Area, Cost, nonForest, ProbDevel, TownChar" 



########################################################


  
print "\n>> DEFINING THE CUSTOM GENERATOR"
def CustomGenerator(random, args):
    nr_inputs = args.get('nr_inputs')
    return_generator = [random.randint(0, 1) for i in range(nr_inputs)]
    #print return_generator
    return return_generator
print "done"


########################################################



print "\n>> DEFINING THE CUSTOM EVALUATOR"

# pick which CFA the algorithm is running
##CFA = sorted(cfaLst)[0] # Fort  
##CFA = sorted(cfaLst)[1] # Mascoma 
##CFA = sorted(cfaLst)[2] # Mill 
##CFA = sorted(cfaLst)[3] # Muddy 
##CFA = sorted(cfaLst)[4] # Salmon 
##CFA = sorted(cfaLst)[5] # Sprague  
print "CFA = {0}".format(CFA)

# identify which CFA the algorithm is investigating
parcelAttributes = cfaDict[CFA]
parcCnt = len(parcelAttributes)

# identify unique parcel field depending on CFA
uniqueID = []
if CFA == sorted(cfaLst)[1] or CFA == sorted(cfaLst)[5]: # Mascoma OR Sprague
    uniqueID.append("NHGIS_ID")
if CFA == sorted(cfaLst)[0] or CFA == sorted(cfaLst)[2]: # For OR Mill
    uniqueID.append("LOC_ID")
if CFA == sorted(cfaLst)[3] or CFA == sorted(cfaLst)[4]: # Muddy OR Salmon
    uniqueID.append("NPARNO")

def CustomEvaluator(candidates, args):
    fitness = []
    maxDevelProb = max(parcelAttributes, key=lambda x: x[4])[4] 
    
    print "NEW GENERATION"
    soln_num = 0
    
    for c in candidates:
        
        soln_num = soln_num + 1
        print "Candidate solution # = {0} of 50".format(soln_num)
        
        # placeholders for final objective values
        tot_area = 0    # Obj 1
        tot_nonfor = 0  # Obj 2
        tot_cost = 0    # Obj 3
        tot_devel = 0  # Obj 4
        tot_charact = 0 # Obj 5
        connectLst = [] # Obj 6

        # placeholder for number of parcels within the solution
        parcelsINsoln = 0

        # iterate through each parcel to calculate objectives
        for parcelID, parcel in enumerate(c):
            
            # placeholders for calculating objectives
            parcelArea = parcelAttributes[parcelID][1] #hectares
            nonforest = parcelAttributes[parcelID][3] #hectares
            cost = parcelAttributes[parcelID][2] #US dollars
            devel = parcelAttributes[parcelID][4] #average developement probability
            charact = parcelAttributes[parcelID][5] #town character alignment (summed)
            connect = parcelAttributes[parcelID][0] #meters           

            # calculate objectives for parcels included (= 1) in the soln
            if parcel == 1:

                # add parcel to count for the solution
                parcelsINsoln = parcelsINsoln + 1

                # add parcel to list for connectivity measure
                connectLst.append(str(connect))

                # Objective 1 = total area
                tot_area = parcelArea + tot_area

                # Objective 2 = total area of non-forest
                tot_nonfor = nonforest + tot_nonfor

                # Objective 3 = total cost
                tot_cost = cost + tot_cost

                # Objective 4 = minimize # parcels with low development probability
                tot_devel = (maxDevelProb - devel) + tot_devel

                # Objective 5 = minimize impact to town character
                tot_charact = charact + tot_charact

        # select parcels included in the solution
        query = "{0} in {1}".format(uniqueID[0], connectLst)
        query = query.replace("[","(")
        query = query.replace("]",")")
        ##################
        infeature = r"P:\GeneticAlgorithm\GADerivedData\CFA_Parcel_Shapefiles\{0}_parcels_zones_MINobj.shp".format(CFA)
        outlayer = r"P:\GeneticAlgorithm\GATemp\solutionLayer"
        arcpy.MakeFeatureLayer_management(infeature, outlayer, where_clause = query)

        # write parcels in solution to new featureclass
        finalshp = r"P:\GeneticAlgorithm\GATemp\solutionShapefile.shp"
        arcpy.CopyFeatures_management(outlayer, finalshp)
        
        # input layer with PAs
        cfaPA = r"P:\GeneticAlgorithm\Data\NearbyPAs\{0}_NearbyPAs.shp".format(CFA)

        #combine selected parcels and surrounding PAs
        output = r"P:\GeneticAlgorithm\GATemp\solutionConnectivity.shp"
        arcpy.Merge_management([finalshp, cfaPA], output)

        # calculate the distances between the parcels included in the solution
        table = r"P:\GeneticAlgorithm\GATemp\neartable_candidateSoln"
        arcpy.GenerateNearTable_analysis(output, output, table)

        # sum distance for connectivity metric
        # Objective 6 = minimize distances within soln to (= maximized connectivity)
        tot_connect = round(0, 2)
        rows = arcpy.SearchCursor(table)
        for row in rows:
            newLine = ""
            neardist = float(row.getValue("NEAR_DIST"))/1000 #m to km
            tot_connect = neardist + tot_connect
        # close cursor
        del row, rows
        
        # avg devel probability for each solution
        avg_devel = 0 if devel == 0 else tot_devel / parcelsINsoln
        
        # change units for area
        tot_area = tot_area * float(0.404686) #acres to hectares
        # NOTE: all of the 2017.08.15 results were based on dividing the area by 0.404686 (rather than multiplying)

        # add objective values to output file
        fitness.append(inspyred.ec.emo.Pareto([tot_area, tot_nonfor, tot_cost, avg_devel, tot_charact, tot_connect], 
                                              maximize = [True, False, False, False, False, False])) 

    return fitness
print "done"



########################################################



print "\n>> RUN THE ALGORITHM"

# time algorithm length
start = time()
print "Start the stopwatch at {0}".format(ctime())

prng = random.Random()
prng.seed(time())

ea = inspyred.ec.emo.NSGA2(prng)
ea.variator = [inspyred.ec.variators.uniform_crossover, 
               inspyred.ec.variators.bit_flip_mutation]
ea.terminator = inspyred.ec.terminators.generation_termination
final_pop = ea.evolve(generator=CustomGenerator, 
                      evaluator=CustomEvaluator,
                      pop_size=50,
                      maximize=True,
                      nr_inputs = parcCnt,
                      max_generations=80)

final_arc = ea.archive
##print('Best Solutions: \n')
##for f in final_arc:
##    print(f.fitness)
##    print(f.candidate)

arcsoln= [a.candidate for a in final_arc]
arcfit= [a.fitness for a in final_arc]

# Write solns and fitnesses to 
combo = zip(arcsoln, arcfit)
import csv
csvfile = r"P:\GeneticAlgorithm\GAResults\6Dsolutions_MIN.csv"

#Assuming res is a list of lists
with open(csvfile, "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(combo)
print "done"

# final time for algorithm run
stop = time()
diff = (stop-start)/60/60
print "Stop the stopwatch at {0}".format(ctime())
print "Final run time = {0} hours".format(diff)
