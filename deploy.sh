chmod 600 deploy_key
rsync -Pav -e "ssh -i deploy_key" --exclude=deploy_key $DEPLOY_HOST:./ ~/shiny/appdir/bernudarzi