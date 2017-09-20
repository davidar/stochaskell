%.data: %.data.gz
	gunzip -k $<

covtype.std.data: covtype.data covtype.py
	python covtype.py > $@

covtype.data.gz:
	wget https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz
