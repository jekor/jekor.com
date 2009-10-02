\section{Gressgraph}

I'm just going to do this right here for now since it's simple.

In order to examine the types of firewalls people are trying to visualize,
we're going to save them to a file (the same with the output).

> module CMS.Gressgraph (graphChains) where

> import CMS.Utils

> import System.IO (openTempFile, hPutStr, hClose)
> import System.Process (system)

Running gressgraph through the web is convenient for others and nice for me,
because it gives me feedback that helps me improve the program in the form of
firewall chains people are trying to graph.

So, we'll write out the chains to a file. However, we don't want to store
multiple copies of the example. This is a bit ugly, but it works.

> example :: String
> example = unlines [
>   "Chain INPUT (policy DROP 1226127 packets, 148780906 bytes)",
>   "    pkts      bytes target     prot opt in     out     source               destination         ",
>   "   57225  8133562 DROP       all  --  any    any     anywhere             anywhere            state INVALID ",
>   "   64147 24178835 DROP       all  --  eth0   any     10.0.0.0/8           anywhere            ",
>   "      13    12264 DROP       all  --  eth0   any     172.16.0.0/255.250.0.0  anywhere            ",
>   "    6191   910525 DROP       all  --  eth0   any     192.168.0.0/16       anywhere            ",
>   "347547935 354119630571 ACCEPT     all  --  any    any     anywhere             anywhere            state RELATED,ESTABLISHED ",
>   "   44925 19199122 ACCEPT     udp  --  ath0   any     anywhere             anywhere            udp dpt:bootps ",
>   "   70995  4434700 ACCEPT     all  --  lo     any     anywhere             anywhere            ",
>   "  136396  8110576 ACCEPT     tcp  --  any    any     anywhere             anywhere            tcp dpt:ssh state NEW ",
>   "      28     1692 ACCEPT     tcp  --  ath0   any     anywhere             anywhere            state NEW tcp dpt:microsoft-ds ",
>   "      79     5020 ACCEPT     tcp  --  ath0   any     anywhere             anywhere            state NEW tcp dpt:netbios-ssn ",
>   "       1       60 ACCEPT     tcp  --  ath0   any     anywhere             anywhere            state NEW tcp dpt:domain ",
>   "  250743 16275259 ACCEPT     udp  --  ath0   any     anywhere             anywhere            state NEW udp dpt:domain ",
>   "    4253   323228 ACCEPT     udp  --  ath0   any     anywhere             anywhere            udp dpt:ntp ",
>   "  144691  7305519 ACCEPT     tcp  --  eth0   any     anywhere             anywhere            tcp dpts:10001:10010 state NEW ",
>   "      75     4784 ACCEPT     tcp  --  ath0   any     neo                  anywhere            tcp dpt:ipp state NEW ",
>   "       0        0 ACCEPT     udp  --  eth0   any     anywhere             anywhere            udp dpt:ntp ",
>   "      73    41304 ACCEPT     udp  --  eth1   any     anywhere             anywhere            udp dpt:bootps ",
>   "      14      939 ACCEPT     udp  --  eth1   any     anywhere             anywhere            udp dpt:domain ",
>   "       0        0 ACCEPT     tcp  --  eth1   any     anywhere             anywhere            tcp dpt:domain ",
>   "       0        0 ACCEPT     udp  --  eth1   any     anywhere             anywhere            udp dpt:ntp ",
>   "       0        0 ACCEPT     tcp  --  ath0   any     malglata             anywhere            state NEW ",
>   "",
>   "Chain FORWARD (policy DROP 209896 packets, 11331181 bytes)",
>   "    pkts      bytes target     prot opt in     out     source               destination         ",
>   "191412264 130555797638 ACCEPT     all  --  any    any     anywhere             anywhere            state RELATED,ESTABLISHED ",
>   " 2457788 184396099 ACCEPT     all  --  ath0   eth0    anywhere             anywhere            ",
>   "       8      576 ACCEPT     all  --  ath0   ath0    necesiga             anywhere            ",
>   "      14     1088 ACCEPT     all  --  ath0   ath0    neo                  anywhere            ",
>   "     117     5616 ACCEPT     all  --  ath0   ath0    malglata             anywhere            ",
>   "    5786   472056 ACCEPT     all  --  eth1   eth0    anywhere             anywhere            ",
>   "",
>   "Chain OUTPUT (policy DROP 3466 packets, 161804 bytes)",
>   "    pkts      bytes target     prot opt in     out     source               destination         ",
>   "274047964 141843115914 ACCEPT     all  --  any    any     anywhere             anywhere            state NEW,RELATED,ESTABLISHED ",
>   "       0        0 ACCEPT     all  --  any    lo      anywhere             anywhere            " ]

> graphChains :: String -> String -> FilePath -> IO FilePath
> graphChains chains format graphDir =
>   let chains' = convertLineEndings chains ++ "\n" in
>   if chains' == example
>     then return $ graphDir </> "example" <.> format
>     else do
>       (chainFile, chainH) <- openTempFile graphDir "graph.iptables"
>       hPutStr chainH chains'
>       hClose chainH
>       system $ "gressgraph < " ++ chainFile ++ " > " ++ (replaceExtension chainFile "twopi")
>       system $ "twopi -T" ++ format ++ " " ++ (replaceExtension chainFile "twopi") ++
>                                      " > " ++ (replaceExtension chainFile format)
>       return $ replaceExtension chainFile format