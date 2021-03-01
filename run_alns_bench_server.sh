#!/bin/bash

SBT_HOME=/etinfo/users2/cthomas/sbt/bin
BIN=/etinfo/users2/cthomas/bin

Date=$(date +"%m-%d-%y")
VNum=`hg id -i`
BenchDir="oscar-perf/src/main/scala/oscar/anytime/lns"
BenchRoot="oscar.anytime.lns"
SbtOutput=`$SBT_HOME/sbt "project oscar-perf" "export runtime:fullClasspath"`
#echo $SbtOutput
CP=${SbtOutput##*$'\n'}
ConfigsFile="$BenchDir/configs.txt"
Out="ALNS-bench-results/$Date-$VNum"
InstancesToRun="parallel-instances.txt"
N=20

echo -e "\n\n\n"
echo "Date -> $Date"
echo "Version Number -> $VNum"
echo "Benchmarks directory -> $BenchDir"
echo "Benchmark root -> $BenchRoot"
echo "Classpath -> $CP"
echo "Configs file -> $ConfigsFile"
echo "Output parameter -> $Out"
echo "instances file -> $InstancesToRun"
echo "N -> $N"
echo -e "\n"

run_search () {
    echo "Starting new search:"
#    echo "Classpath: $1"
#    echo "Output: $2"
    echo "Instance: $3"
    echo "Config: $4"
    scala -J-Xmx2g -cp $1 $3 $4 --out $2
    echo -e "\n\n\n"
}

export -f run_search

for d in `ls ${BenchDir}/benchmarks`; do
    for d2 in `ls ${BenchDir}/benchmarks/${d}`; do
        if [ "${d2: -6}" == ".scala" ]; then
            f=${d2%%??????}
            echo "$BenchRoot.benchmarks.$d.$f" >> ${InstancesToRun}
        elif [ -d "$BenchDir/benchmarks/$d/$d2" ]; then
            for d3 in `ls ${BenchDir}/benchmarks/${d}/${d2}`; do
                if [ "${d3: -6}" == ".scala" ]; then
                    f=${d3%%??????}
                    echo "$BenchRoot.benchmarks.$d.$d2.$f" >> ${InstancesToRun}
                fi
            done
        else
            echo "file $d2 ignored"
        fi
    done
done

echo -e "\nConfigurations:"
cat ${ConfigsFile}
echo -e "\nInstances:"
cat ${InstancesToRun}
echo -e "\n\n\n"

for ((i=1; i<=$N; i++)); do
    echo "Run $i:";
    $BIN/parallel --gnu --jobs 90% run_search ${CP} ${Out} :::: ${InstancesToRun} :::: ${ConfigsFile};
done

rm ${InstancesToRun}

cp -r --parents "${Out}" "/etinfo/users2/cthomas/Workspace/"

#scala -J-Xmx4g -cp ${CP} ${BenchRoot}.utils.HtmlReporter ${Out}

#cat "${Out}/${Date}-${VNum}_htmlReport.html" > "/etinfo/users2/cthomas/Workspace/${Out}/htmlReport.html"
