# featuresearch
Find features in CSV datasets formatted as:
```
...
<category> <feature1> <feature2> <feature3> ... <featureN>
...
```

Inlcudes a small command line interface:
```
Usage:
	featuresearch --help			    print help
	featuresearch data.txt			    run forward selection on data.txt
	featuresearch backward data.txt		run backward elimination on data.txt
	featuresearch forward data.txt		run forward selection on data.txt
```

## How to run
[stack](https://docs.haskellstack.org/en/stable/) is required to build and test xtile. It will install GHC and all necessary dependencies.
1. Clone code with ```git clone https://github.com/mgurga/featuresearch/``` and enter directory ```cd featuresearch```
2. Build the project with ```stack build```
3. Run the project with ```stack run```, this will solve a random puzzle and print the results
4. Run ```stack run help``` to see usage
5. Run tests with ```stack test```
