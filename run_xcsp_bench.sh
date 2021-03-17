#!/bin/bash

#SCALA=/home/oscar/scala-2.11.8/bin
SBT_HOME=/etinfo/users2/cthomas/sbt/bin
BIN=/etinfo/users2/cthomas/bin

ReRun="true"
Timeout=240
Memory=1000

Date=$(date +"%m-%d-%y")
VNum=`hg id -i`
BenchDir="data/xcsp3/xcspBench"
SolversDir="oscar-cp-xcsp3/src/main/scala/oscar/xcsp3/competition/solvers"
JarsDir="xcsp-competition-jars"
SolversRoot="oscar.xcsp3.competition.solvers"
SbtOutput=`$SBT_HOME/sbt "project oscar-cp-xcsp3" "update" "compile" "export runtime:fullClasspath"`
#SbtOutput=`sbt "project oscar-cp-xcsp3" "update" "compile" "export runtime:fullClasspath"`
#echo $SbtOutput
CP=${SbtOutput##*$'\n'}
#CP="None"
Out="XCSP-bench-results"

Solvers="solvers.txt"
ParallelSolvers="solvers_parallel.txt"
Jars="jars_.txt"
ParallelJars="jars_parallel.txt"
CopInstances="cop_instances.txt"
CspInstances="csp_instances.txt"

echo -e "\n\n\n"
echo "Version Number -> $VNum"
echo "Benchmarks directory -> $BenchDir"
echo "Classpath -> $CP"
echo "Output directory -> $Out"
echo -e "\n"

run_search () {
    echo "Starting new search:"
#    echo "Classpath: $1"
#    echo "Output: $2"
    InstanceName=${6##*/}
    InstanceType=`basename "$(dirname $6)"`
    SolverName=${7##*.}
    echo "Instance: $InstanceName"
    echo "Type: $InstanceType"
    echo "Solver: $SolverName"

    OutPath="${2}/${3}/${InstanceName%.*}-${SolverName}.txt"

    if [ -e "$OutPath" ]
    then
        echo "$OutPath already exists!"
        if [ "$ReRun" = "true" ]
        then
            echo "Re-running bench"
            echo "0 c instance: $InstanceName" > $OutPath
            echo "0 c type: $InstanceType" >> $OutPath
            echo "0 c solver: $SolverName" >> $OutPath
            scala -J-Xmx${5}m -cp $1 $7 --timelimit $4 --nbcore 4 $6 >> $OutPath
        fi
    else
        echo "Running bench"
        echo "0 c instance: $InstanceName" > $OutPath
        echo "0 c type: $InstanceType" >> $OutPath
        echo "0 c solver: $SolverName" >> $OutPath
        scala -J-Xmx${5}m -cp $1 $7 --timelimit $4 --nbcore 4 $6 >> $OutPath
    fi

    echo -e "\n\n\n"
}
export -f run_search

run_jar () {
    echo "Starting new search:"
#    echo "Classpath: $1"
#    echo "Output: $2"
    InstanceName=${6##*/}
    InstanceType=`basename "$(dirname $6)"`
    SolverName=${7##*/}
    echo "Instance: $InstanceName"
    echo "Type: $InstanceType"
    echo "Solver: $SolverName"

    OutPath="${2}/${3}/${InstanceName%.*}-${SolverName%.*}.txt"

    if [ -e "$OutPath" ]
    then
        echo "$OutPath already exists!"
        if [ "$ReRun" = "true" ]
        then
            echo "Re-running bench"
            echo "0 c instance: $InstanceName" > $OutPath
            echo "0 c type: $InstanceType" >> $OutPath
            echo "0 c solver: $SolverName" >> $OutPath
            java -jar -Xmx${5}m $7 --timelimit $4 --nbcore 4 $6 >> $OutPath
        fi
    else
        echo "Running bench"
        echo "0 c instance: $InstanceName" > $OutPath
        echo "0 c type: $InstanceType" >> $OutPath
        echo "0 c solver: $SolverName" >> $OutPath
        java -jar -Xmx${5}m $7 --timelimit $4 --nbcore 4 $6 >> $OutPath
    fi

    echo -e "\n\n\n"
}

export -f run_jar

for s in `ls ${SolversDir}`; do
    if [ "${s: -6}" == ".scala" ]; then
        f=${s%%??????}
        echo "$SolversRoot.$f" >> ${Solvers}
    elif [ "$s" == parallel ]; then
        for s2 in `ls ${SolversDir}/${s}`; do
            if [ "${s2: -6}" == ".scala" ]; then
                f=${s2%%??????}
                echo "$SolversRoot.$s.$f" >> ${ParallelSolvers}
            else
                echo "$s2 ignored!"
            fi
        done
    else
        echo "$s ignored!"
    fi
done

for s in `ls ${JarsDir}`; do
    if [ "${s: -4}" == ".jar" ]; then
        echo "$JarsDir/$s" >> ${Jars}
    elif [ "$s" == parallel ]; then
        for s2 in `ls ${JarsDir}/${s}`; do
            if [ "${s2: -4}" == ".jar" ]; then
                echo "$JarsDir/$s/$s2" >> ${ParallelJars}
            else
                echo "$s2 ignored!"
            fi
        done
    else
        echo "$s ignored!"
    fi
done

for i in `ls ${BenchDir}/cop`; do
    if [ "${i: -4}" == ".xml" ]; then
        echo "$BenchDir/cop/$i" >> ${CopInstances}
    else
        echo "file $i is not a xml file!"
    fi
done

for i in `ls ${BenchDir}/csp`; do
    if [ "${i: -4}" == ".xml" ]; then
        echo "$BenchDir/csp/$i" >> ${CspInstances}
    else
        echo "file $i is not a xml file!"
    fi
done

echo -e "\nSolvers:"
cat ${Solvers}
echo -e "\nParallel solvers:"
cat ${ParallelSolvers}
echo -e "\nJars:"
cat ${Jars}
echo -e "\nParallel jars:"
cat ${ParallelJars}
echo -e "\nCop Instances:"
cat ${CopInstances}
echo -e "\nCsp Instances:"
cat ${CspInstances}
echo -e "\n\n\n"

if [ ! -e "${Out}/${VNum}" ]
then
    echo "Creating directory: ${Out}/${VNum}"
    mkdir "${Out}/${VNum}"
fi

#$BIN/parallel --gnu --jobs 75% run_search ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CopInstances} :::: ${Solvers}
#$BIN/parallel --gnu --jobs 20% run_search ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CopInstances} :::: ${ParallelSolvers}

$BIN/parallel --gnu --jobs 75% run_jar ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CopInstances} :::: ${Jars}
$BIN/parallel --gnu --jobs 20% run_jar ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CopInstances} :::: ${ParallelJars}

#$BIN/parallel --gnu --jobs 75% run_search ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CspInstances} :::: ${Solvers}
#$BIN/parallel --gnu --jobs 20% run_search ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CspInstances} :::: ${ParallelSolvers}

$BIN/parallel --gnu --jobs 75% run_jar ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CspInstances} :::: ${Jars}
$BIN/parallel --gnu --jobs 20% run_jar ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${CspInstances} :::: ${ParallelJars}

rm ${CopInstances}
rm ${CspInstances}
rm ${Solvers}
rm ${ParallelSolvers}
rm ${Jars}
rm ${ParallelJars}

cp -r --parents "${Out}/${VNum}" "/etinfo/users2/cthomas/Workspace/"

scala -J-Xmx2g -cp ${CP} oscar.xcsp3.competition.html.HtmlReporter ${Out}

cat "${Out}/htmlReport.html" > "/etinfo/users2/cthomas/Workspace/${Out}/htmlReport.html"
