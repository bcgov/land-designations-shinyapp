node('master') {

	stage('build') {
	 echo "Building..."
	 openshiftBuild bldCfg: 'rshiny', showBuildLogs: 'true'
	 openshiftTag destStream: 'rshiny-moe-land-designations', verbose: 'true', destTag: '$BUILD_ID', srcStream: 'rshiny-moe-land-designations', srcTag: 'latest'
	 openshiftTag destStream: 'rshiny-moe-land-designations', verbose: 'true', destTag: 'dev', srcStream: 'rshiny-moe-land-designations', srcTag: 'latest'
    }

}


stage('deploy-test') {
  input "Deploy to test?"

  node('master'){
     openshiftTag destStream: 'rshiny-moe-land-designations', verbose: 'true', destTag: 'test', srcStream: 'rshiny-moe-land-designations', srcTag: '$BUILD_ID'
  }
}

stage('deploy-prod') {
  input "Deploy to prod?"
  node('master'){
     openshiftTag destStream: 'rshiny-moe-land-designations', verbose: 'true', destTag: 'prod', srcStream: 'rshiny-moe-land-designations', srcTag: '$BUILD_ID'
  }

}

