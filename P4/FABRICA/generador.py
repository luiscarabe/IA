import numpy
import subprocess
import sys, os

def main():
	fichero = open(sys.argv[1], 'w')
	
	for i in range(1,99999):
		valores = numpy.random.randint(0, high = 200, size = 6)
		
		resultado = ''+str(valores[0])+" "+str(valores[1])+" " + str(valores[2]) +" " + str(valores[3]) +" " + str(valores[4]) +" " + str(valores[5]) + "\t"
		
		cadena = resultado + "Valor i: " + str(i) +"/99999"
		print (cadena)
				
		script = subprocess.run(['./probador.sh', str(valores[0]), str(valores[1]), str(valores[2]),str(valores[3]), str(valores[4]), str(valores[5])], stdout=subprocess.PIPE)
		
		resultado += script.stdout.decode('utf-8')
		#resultado += "\n"
		
		fichero.write(resultado)
		
		
		
main()
