git clone https://github.com/EpistasisLab/regression-benchmark.git
cd regression-benchmark
git checkout -f 07a71de3ed8e7009f08c5a21bcbf17276b0c946b
cd ..
echo f | xcopy /s ".\regression-benchmark\postprocessing\friedman.r" ".\friedman.r"
@RD /S /Q ".\regression-benchmark"