#!/bin/bash
##################################################
## John Massey
## Attain
## 3/29/2017
##
## Configure local scratch directory and
##   symlink back to /modeling/<project>/tmp/
##
## usage srun --ntasks-per-node=1 --ntasks $NUM_NODES user_scratch_setup.sh $PROJECT_HOME
##  Run BEFORE any other SLURM tasks as part of the P532/p600 models or variants
##################################################
#
# 1. Get model version from Project Home Env Variable and configure variables
hostname=`hostname`
projectHome=$1
project=`basename -a $projectHome`
uname=`whoami`
scratchDir="/scratch/$project/tmp/$uname-scratch"
projDirs=( $(find $projectHome -maxdepth 1 -type d -printf '%P\n') )

# 2. Check Scratch/<project> exists
if [ ! -d "/scratch/$project" ]; then
    mkdir /scratch/$project
    cd /scratch/$project
    echo "Created directory /scratch/$project on $hostname for $uname"
else
    echo "/scratch/$project already Exists on $hostname for $uname"
    echo " "
    cd /scratch/$project
fi

# Get all Directories in the Project Directory.
#projDirs=( $(find $projectHome -maxdepth 1 -type d -printf '%P\n') )
#cd /scratch/$project

# 3. Test to see if symlink exists, if not create.
# tmp is a special case to allow for local file processing.
for dir in "${projDirs[@]}"; do
    if [ "$dir" == "tmp" ]; then
       if [ ! -d "/scratch/$project/$dir" ]; then
          mkdir /scratch/$project/$dir
          chmod 777 /scratch/$project/$dir
          cd /scratch/$project/$dir
          echo "Created directory /scratch/$project on $hostname for $uname"
 
          ln -s /modeling/$project/$dir/uci
          ln -s /modeling/$project/$dir/wdm

	  # Create user Scratch Directory for Local processing
          if [ ! -d "$scratchDir" ]; then
              mkdir $scratchDir
              echo "Created directory $scratchDir on $hostname for $uname"
          else 
              echo "$scratchDir already Exists on $hostname for $uname"
          fi

          cd /scratch/$project
       else 
          echo "Directory /scratch/$project/$dir exists on $hostname"
          # Test symlinks anyway and create if necessary
          cd /scratch/$project/$dir

          if [ ! -L "/scratch/$project/$dir/uci" ]; then
             ln -s /modeling/$project/$dir/uci
             echo "Created Symlink /scratch/$project/$dir/uci on $hostname for $uname"
          else
             echo "Symlink /modeling/$project/$dir/uci already exists on $hostname"
          fi

          if [ ! -L "/scratch/$project/$dir/wdm" ]; then
             ln -s /modeling/$project/$dir/wdm
             echo "Created Symlink /scratch/$project/$dir/wdm on $hostname for $uname"
          else
             echo "Symlink /modeling/$project/$dir/wdm already exists on $hostname"
          fi

          cd /scratch/$project
       fi

    #check all other dir's to ensure they are properly symlinked
    else 
        if [ ! -L "/scratch/$project/$dir" ]; then
           ln -s /modeling/$project/$dir
           echo "Created Symlink /scratch/$project/$dir on $hostname for $uname"
        else
           echo "Symlink /scratch/$project/$dir exists on $hostname"
        fi
    fi
done

# 4. Set Permissions on all directories we just created leaving out symlinks
find /scratch/$project -maxdepth 1 -type d -exec chown $uname:modeling {} \;
find /scratch/$project -maxdepth 1 -type d -exec chmod 777 {} \;
echo "Set Permissions and Ownership for /scratch/$project on $hostname for $uname recursively"
#EOF
