#!/bin/bash

CHANGELOGFILE="CHANGELOG.md"
SKIP_CHANGELOG_LBL="skip-changelog"


# Check if changelog has been modified
git fetch origin $CI_MERGE_REQUEST_TARGET_BRANCH_NAME
DIFF=$(git diff --name-only FETCH_HEAD...HEAD)

if echo "$DIFF" | grep -q $CHANGELOGFILE; then
    echo "[x] $CHANGELOGFILE has been edited."
    exit 0
fi

# if changelog file has not been modified, checking if the skip-changelog label is applied to the MR
echo "$CHANGELOGFILE not modified. Verifying $SKIP_CHANGELOG_LBL label"

# Checking labels of MR through gitlab api
MR_LABELS=$(curl --retry 2 --header "JOB-TOKEN: $CI_JOB_TOKEN" "$CI_API_V4_URL/projects/$CI_PROJECT_ID/merge_requests/$CI_MERGE_REQUEST_IID" | jq -r '.labels[]')

if echo "$MR_LABELS" | grep -qi $SKIP_CHANGELOG_LBL; then
    echo "[x] $SKIP_CHANGELOG_LBL detected, Merge Request accepted"
    exit 0
else
    echo "/!\ File $CHANGELOGFILE has not been modified but $SKIP_CHANGELOG_LBL is not assigned to the Merge Request"
    echo "Modify $CHANGELOGFILE or apply $SKIP_CHANGELOG_LBL to the Merge Request"
    exit 1
fi