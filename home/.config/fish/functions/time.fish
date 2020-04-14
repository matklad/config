# Defined in - @ line 0
function time --description alias\ time=time\ --format\ \'real\ \%es\\nuser\ \%Us\\nsys\ \ \%Ss\\nrss\ \ \%Mk\'
	command time --format 'real %es\nuser %Us\nsys  %Ss\nrss  %Mk' $argv;
end
