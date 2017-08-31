node('master') {
   
	stage('build') {
	 echo "Building..."
	 openshiftBuild bldCfg: 'rshiny', showBuildLogs: 'true'
	 openshiftTag destStream: 'rshiny', verbose: 'true', destTag: '$BUILD_ID', srcStream: 'rshiny', srcTag: 'latest'
	 openshiftTag destStream: 'rshiny', verbose: 'true', destTag: 'dev', srcStream: 'rshiny', srcTag: 'latest'
    }
	
}


stage('deploy-test') {
  input "Deploy to test?"
  
  node('master'){
     openshiftTag destStream: 'rshiny', verbose: 'true', destTag: 'test', srcStream: 'rshiny', srcTag: '$BUILD_ID'
  }
}

stage('deploy-prod') {
  input "Deploy to prod?"
  node('master'){
     openshiftTag destStream: 'rshiny', verbose: 'true', destTag: 'prod', srcStream: 'rshiny', srcTag: '$BUILD_ID'
  }
  
}

