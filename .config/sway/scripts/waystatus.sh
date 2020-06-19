#!/bin/bash

# This function parses /proc/net/dev file searching for a line containing $interface data.
# Within that line, the first and ninth numbers after ':' are respectively the received and transmited bytes.
function get_bytes {
   # Find active network interface
   interface=$(ip route get 8.8.8.8 2>/dev/null| awk '{print $5}')
   line=$(grep $interface /proc/net/dev | cut -d ':' -f 2 | awk '{print "received_bytes="$1, "transmitted_bytes="$9}')
   eval $line
   now=$(date +%s%N)
}

# Function which calculates the speed using actual and old byte number.
# Speed is shown in KByte per second when greater or equal than 1 KByte per second.
# This function should be called each second.
function get_velocity {
   value=$1
   old_value=$2
   now=$3

   timediff=$(($now - $old_time))
   velKB=$(echo "1000000000*($value-$old_value)/1024/$timediff" | bc)
   if test "$velKB" -gt 1024; then
      echo $(echo "scale=2; $velKB/1024" | bc)MB/s
   else
      echo ${velKB}KB/s
   fi
}

print_volume() {
   volume="$(pamixer --get-volume)"
   if test "$volume" -gt 0; then
      echo -e "VOL:${volume}"
   else
      echo -e "Mute"
   fi
}

print_date(){
   date "+%T"
}



# Initial values
get_bytes
old_received_bytes=$received_bytes
old_transmitted_bytes=$transmitted_bytes
old_time=$now

while true
do

   # Get new transmitted, received byte number values and current time
   get_bytes

   # Calculates speeds
   vel_recv=$(get_velocity $received_bytes $old_received_bytes $now)
   vel_trans=$(get_velocity $transmitted_bytes $old_transmitted_bytes $now)

   # print the status bar
   echo -n "  $vel_recv $vel_trans  $(print_volume)  $(print_date)  "

   # Update old values to perform new calculations
   old_received_bytes=$received_bytes
   old_transmitted_bytes=$transmitted_bytes
   old_time=$now

   sleep 1

done 
