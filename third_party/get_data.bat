git clone https://github.com/quaquel/EMAworkbench.git
cd EMAworkbench
git checkout 0ef7aef85b50f611702c4a41e82e37684a9e831e
cd ..
echo f | xcopy /s ".\EMAworkbench\ema_workbench\examples\data\bryant et al 2010 data.csv" ".\bryant_et_al_2010_data.csv"
@RD /S /Q ".\EMAworkbench"
git clone https://github.com/quaquel/lake_problem.git
cd lake_problem
git checkout cfb3da0437e59b16d6f6e87e32b33540ef12df3b
cd ..
echo f | xcopy /s ".\lake_problem\data\1000 scenarios 4 policies.tar.gz" ".\1000_scenarios_4_policies.tar.gz"
@RD /S /Q ".\lake_problem"
mkdir ".\tmp"
tar xvzf ".\1000_scenarios_4_policies.tar.gz" -C ".\tmp"
echo f | xcopy /s ".\tmp\experiments.csv" ".\experiments.csv"
echo f | xcopy /s ".\tmp\max_P.csv" ".\max_P.csv"
@RD /S /Q ".\tmp"
del ".\1000_scenarios_4_policies.tar.gz" 
mkdir ".\original"
echo f | xcopy ".\experiments.csv" ".\original\experiments.csv"
echo f | xcopy ".\bryant_et_al_2010_data.csv" ".\original\bryant_et_al_2010_data.csv"
echo f | xcopy ".\max_P.csv" ".\original\max_P.csv"
del ".\experiments.csv" 
del ".\bryant_et_al_2010_data.csv" 
del ".\max_P.csv" 
