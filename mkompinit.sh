#!/bin/sh
echo "#!/bin/sh" > ompinit
echo "mkdir -p ~/.omp" >> ompinit
echo "ln -f -s $1 ~/.omp/images" >> ompinit
echo "touch ~/.omp/scorelist" >> ompinit
