FROM nginx
COPY src/_site /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf
