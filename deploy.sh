chmod 600 deploy_key
sed -i "s/##last_update_date##/ `date '+%d.%m.%Y'`/" app.R
rsync -Pav -e "ssh -o StrictHostKeyChecking=no -i deploy_key" --exclude-from=./rsync-exclude.txt ./ $DEPLOY_HOST:~/shiny/appdir/bernudarzi --delete