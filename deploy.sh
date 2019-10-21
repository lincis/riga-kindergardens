chmod 600 id_rsa
rsync -Pav -e "ssh -i id_rsa" --exclude=id_rsa $DEPLOY_HOST:./ ~/shiny/appdir/bernudarzi