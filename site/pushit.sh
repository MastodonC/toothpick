#! /bin/bash

aws --profile mastodonc s3 sync . s3://openhealthdata.cdehub.org/ --acl public-read --exclude '.git*' --exclude '*.DS_Store' --exclude '*~' --exclude pushit.sh --delete
