default :
	@echo "targets are:"
	@echo "    test"

test :
	R --vanilla < test.d/test.R 

# eof
