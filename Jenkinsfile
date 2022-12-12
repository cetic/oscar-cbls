node {
    env.JAVA_HOME="${tool 'openjdk-11'}"
    env.PATH="${env.JAVA_HOME}/bin:${env.PATH}"
    env.SBT_HOME = "${tool 'sbt-1.7.3'}"

    email_to = "oscar.cbls-cetic@lists.cetic.be"

    try{
    stage('print') {
        sh 'printenv'
    }
        stage('scm') {
            checkout_result = checkout scm: [
                $class: 'MercurialSCM',
                source: 'https://bitbucket.org/oscarlib/oscar',
                revision: env.BRANCH_NAME,
                revisionType: 'BRANCH',
                clean: true
            ],
            poll: false
        }

        def MERCURIAL_REVISION = sh script: 'hg id -i', returnStdout: true

        env.PARAMS = "-Dcetic -DBRANCH_NAME='${env.BRANCH_NAME}' -DREVISION_ID='${MERCURIAL_REVISION}' -DBUILD_ID='${env.BUILD_ID}'"
        env.SBT_CMD = "${env.SBT_HOME}/bin/sbt -Dsbt.log.noformat=true ${env.PARAMS}"


        stage('Build') {
            sh "${env.SBT_CMD} compile"
        }
        stage('Test') {
            sh "${env.SBT_CMD} test"
        }
        stage('Package') {
            sh "${env.SBT_CMD} package -Xdisable-assertions"
        }
        stage('Publish') {
            sh "${env.SBT_CMD} publish -Xdisable-assertions"
        }

    }
    catch (err) {
        currentBuild.result = "FAILURE"

        step([$class: 'Mailer',
           notifyEveryUnstableBuild: true,
           recipients: "${email_to}",
           sendToIndividuals: true])

        throw err
    }
}
