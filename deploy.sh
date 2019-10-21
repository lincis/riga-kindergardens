chmod 600 deploy_key
rsync -Pav -e "ssh -o StrictHostKeyChecking=no -i deploy_key" --exclude-from=./rsync-exclude.txt ./ $DEPLOY_HOST:~/shiny/appdir/bernudarzi