server {
  listen        *:80;
  server_name   jekor.com;
  access_log    /var/log/nginx/jekor.com.access_log main;
  error_log     /var/log/nginx/jekor.com.error_log info;
  root  /home/chris/project/jekor.com;

  location ~* \.(css|js|png|jpg|gif)$ {
    expires 7d;
  }

  location ~* \.(pdf|gz)$ {
  }

  location ~* ^/(emacs|gressgraph|xtee|log2rotate)/ {
  }

  location / {
    include     /etc/nginx/fastcgi_params;
    fastcgi_pass     127.0.0.1:100034;
  }
}

server {
  listen        *:80;
  server_name   www.jekor.com;
  rewrite ^(.*) http://jekor.com$1 permanent;
}
