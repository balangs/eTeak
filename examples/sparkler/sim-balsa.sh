
# BALSAC="/mnt/scratch/bardsley/balsa/build/trunk/balsa/src/balsa-c/balsa-c -I /home/amulinks/balsa/linux/4.0/share"
BALSAC=balsa-c

${BALSAC} -b debug.balsa
${BALSAC} -b types.balsa
${BALSAC} -b buffer.balsa
${BALSAC} -b fetch.balsa
${BALSAC} -b decode.balsa
${BALSAC} -b shifter.balsa
${BALSAC} -b alu.balsa
${BALSAC} -b regbank.balsa
${BALSAC} -b execute.balsa
${BALSAC} -b memarb.balsa
${BALSAC} -b sparkler.balsa
${BALSAC} -b sim.balsa

breeze-sim --fast sim 2>&1
