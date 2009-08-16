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

  location ~* ^/(emacs|gressgraph|xtee)/ {
  }

  location / {
    include     /etc/nginx/fastcgi_params;
    if ($request_method = POST) {
      fastcgi_pass      127.0.0.1:10034;
      break;
    }
    if ($is_args) {
      fastcgi_pass      127.0.0.1:10034;
      break;
    }
    default_type      "text/html; charset=utf-8";
    set $memcached_key "jekor.com:$request_uri";
    memcached_pass    127.0.0.1:11211;
    error_page        404 = /fastcgi;
  }

  location /fastcgi {
    include     /etc/nginx/fastcgi_params;
    fastcgi_pass        127.0.0.1:10034;
  }
}

server {
  listen        *:80;
  server_name   www.jekor.com;
  rewrite ^(.*) http://jekor.com$1 permanent;
}
