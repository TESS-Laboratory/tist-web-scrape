build-pipeline: 
	@echo "🌱🌺🌲🌻🌳 Initiating TIST web-scraping pipeline 🌳🌻🌲🌺🌱"
	@echo "🏗️ Building pipeline... 🏗️"
	@echo "👀 To inspect progress, open the 'targets-run.log' file 👀"
	@nohup Rscript targets-run.R > targets-run.log 2>&1 &