library(ggplot2)

##### Pool plots
#number of pgs per pool and name
p1 <- ggplot(ceph_osd_pool_stats, aes(pool_name, pg_num)) +
	geom_bar(stat='identity',colour='black',fill='white') +
	xlab("pool") + 
	theme(legend.position = 'none', axis.text.x = element_text(size=9, angle = 45, vjust = 1, hjust = 1, face='bold'))+
        ylab("number of pgs") +
	ggtitle(paste('Placement groups per pool- total pgs:',numpgs,'total pools:', numpools))
 

#amount of data in each pool
p2 <- ggplot(ceph_pool_stats, aes(pool_name, num_bytes/(10**12))) +
        geom_bar(stat='identity',colour='black',fill='white') +
        xlab("pool") +
        theme(legend.position = 'none', axis.text.x = element_text(size=9, angle = 45, vjust = 1, hjust = 1, face='bold'))+
        ylab("amount of data (TB)") +
        ggtitle(paste('Amount of data per pool- total data stored (TB)\n replicated:',round(osd_raw_usedTB),'actual:',round(osd_usable_usedTB)))


#fractional amount of pgs allocated vs fractional amount of total data stored (per pool)
osd_pool <- merge(ceph_osd_pool_stats, ceph_pool_stats)
p3 <- ggplot(osd_pool, aes(num_bytes/osd_usable_usedTB/(10**12), pg_num/numpgs)) +
	geom_point() +geom_text(aes(label= ifelse(pg_num/numpgs>.12,as.character(pool_name),'')),hjust=.4, vjust=0) +
	geom_text(aes(label= ifelse((num_bytes/osd_usable_usedTB/(10**12)) >.12,as.character(pool_name),'')),hjust=.4, vjust=0) +
	geom_abline(linetype=2)+
	ylim(c(0,1)) + xlim(c(-.1,1.1))+
	xlab("fraction of data contained") +
	ylab("fraction of pgs allotted") +
	ggtitle("PGs allotted vs amount of data contained per pool") 

#number of object copies in each pool
p4 <- ggplot(osd_pool, aes(pool_name, num_object_copies)) +
        geom_bar(stat='identity',colour='black',fill='white') +
        xlab("pool") +
        theme(legend.position = 'none', axis.text.x = element_text(size=9, angle = 45, vjust = 1, hjust = 1, face='bold'))+
        ylab("number of object copies") +
        ggtitle(paste('Number of object copies per pool\n total objects:',sum(osd_pool$num_object_copies),'total pools:', numpools))

##### PG plots
ceph_pg_stats$poolid <- as.integer(lapply(strsplit(ceph_pg_stats$pgid, split="[.]"), "[",1))

#histogram of data stored per pg
numpgsempty = length(ceph_pg_stats$num_bytes[ceph_pg_stats$num_bytes=='0'])
numpgsnoobj = length(ceph_pg_stats$num_object_copies[ceph_pg_stats$num_object_copies=='0'])

p5 <- ggplot(ceph_pg_stats, aes(x=num_bytes/(10**9))) +
	geom_histogram(colour="black", fill="white") +
	xlab("amount of data stored per pg (GB)") +
	ggtitle(paste("Histogram of amount of data stored on placement groups\n # pgs with no data = ",numpgsempty, ", fraction pgs with no data = ", round(numpgsempty/numpgs,digits=2)) )

p6 <- ggplot(ceph_pg_stats, aes(x=num_object_copies)) +
	geom_histogram(colour="black", fill="white") +
        xlab("number of object copies") +
        ggtitle(paste("Histogram of number of object copies stored on placement groups\n # pgs with no objects = ",numpgsnoobj, ", fraction pgs with no objects = ", round(numpgsnoobj/numpgs,digits=2)) )

##### PG to OSD connection
chosenosds <- unlist(ceph_pg_stats$up_osds)
p7 <- ggplot(data.frame(chosenosds), aes(chosenosds)) + 
	geom_histogram(colour="black", fill="white") +
	#geom_bar(stat='identity',colour='black',fill='white') +
        xlab('OSD ID') +
	ggtitle('histogram of times an OSD has been assigned a placement group')


# same as p14
#p8 <- ggplot(ceph_pg_stats, aes(up_primary)) + 
#	geom_histogram(colour="black", fill="white") +
#	xlab('OSD ID') +
#	ggtitle('histogram of times an OSD has been assigned as primary for a pg')
#

##### OSD plots
#fill level
sum_fill_fraction <- round(summary(ceph_osd_stats$fill_fraction),2)
p9 <- ggplot(ceph_osd_stats, aes(x=fill_fraction)) + 
	geom_histogram(colour="black", fill="white") +
	xlab('OSD fill level') +
	ggtitle(paste0('histogram of OSD fill levels\nmin: ', sum_fill_fraction[1],', 1Q: ',sum_fill_fraction[2],', Med: ',sum_fill_fraction[3],', Mean: ',sum_fill_fraction[4],', 3Q: ', sum_fill_fraction[5],', Max: ',sum_fill_fraction[6]))

#performance- latency
sum_commit_latency <- summary(ceph_osd_stats$commit_latency_ms)
p10 <- ggplot(ceph_osd_stats, aes(x=commit_latency_ms)) +
        geom_histogram(colour="black", fill="white") +
        xlab('OSD commit latency (ms)') +
        ggtitle(paste('histogram of OSD commit latency\nmin:', sum_commit_latency[1],', 1Q:',sum_commit_latency[2],', Med:',sum_commit_latency[3],', Mean:',sum_commit_latency[4],', 3Q:', sum_commit_latency[5],', Max:',sum_commit_latency[6]))

sum_apply_latency <- summary(ceph_osd_stats$apply_latency_ms)
p11 <- ggplot(ceph_osd_stats, aes(x=apply_latency_ms)) +
        geom_histogram(colour="black", fill="white") +
        xlab('OSD apply latency (ms)') +
        ggtitle(paste('histogram of OSD apply latency\nmin:', sum_apply_latency[1],', 1Q:',sum_apply_latency[2],', Med:',sum_apply_latency[3],', Mean:',sum_apply_latency[4],', 3Q:', sum_apply_latency[5],', Max:',sum_apply_latency[6]))

#ceph_osd_stats[ceph_osd_stats$apply_latency_ms ==max(ceph_osd_stats$apply_latency_ms), ]
#ceph_pg_stats[ceph_pg_stats$up_primary ==331,]


require(plyr)
osd_objbytes <- ddply(ceph_pg_stats, .(up_primary), summarize, num_pgs=length(up_primary), tot_bytes=sum(num_bytes), tot_object_copies=sum(num_object_copies))

numobject_copies <- sum(osd_objbytes$tot_object_copies)
#number of objects an OSD is primary for	
p12 <- ggplot(osd_objbytes, aes(up_primary, tot_object_copies)) +
        geom_bar(stat='identity',colour='black',fill='white') +
        xlab("OSD ID") +
        ylab("number of object copies") +
        ggtitle(paste('Object copies per OSD- total object copies:',numobject_copies))

numbytes <- round(sum(osd_objbytes$tot_bytes)/(10**12),2)
#size of objects an OSD is primary for	
p13 <- ggplot(osd_objbytes, aes(up_primary, tot_bytes/(10**9))) +
        geom_bar(stat='identity',colour='black',fill='white') +
        xlab("OSD ID") +
        ylab("total data stored (GB)") +
        ggtitle(paste('Data size per OSD- total data (TB):',numbytes))

#var(osd_objbytes$tot_bytes/10**9)
#sd(osd_objbytes$tot_bytes/10**9)
#mean(osd_objbytes$tot_bytes/10**9)

#number of times an OSD is primary
p14 <- ggplot(osd_objbytes, aes(up_primary, num_pgs)) +
        geom_bar(stat='identity',colour='black',fill='white') +
        xlab("OSD ID") +
        ylab("number of times primary for a PG") +
        ggtitle(paste('Number of times an OSD is serving as primary for a PG'))

#hist of above
p15 <- ggplot(osd_objbytes, aes(x=num_pgs)) +
        geom_histogram(colour='black',fill='white') +
        xlab("number of placement groups assigned as primary") +
        ggtitle(paste('Histogram of number of PGs an OSD is serving as primary for'))


ceph_osd_stats_more <- merge(ceph_osd_stats, osd_objbytes, by.x = 'osd', by.y = 'up_primary', all=TRUE)
ceph_osd_stats_more[is.na(ceph_osd_stats_more)] <-  0 

# OSD latency trends

p16 <- ggplot(ceph_osd_stats_more, aes(x=commit_latency_ms, y=kb_used/(10**9))) +
	geom_point() +
	xlab('commit latency (ms)') +
	ylab('data stored (TB)') +
	ggtitle('OSD commit latency and data size')

p17 <- ggplot(ceph_osd_stats_more, aes(x=apply_latency_ms, y=kb_used/(10**9))) +
        geom_point() +
        xlab('apply latency (ms)') +
        ylab('data stored (TB)') +
        ggtitle('OSD apply latency and data size')

p18 <- ggplot(ceph_osd_stats_more, aes(x=commit_latency_ms, y=num_pgs)) +
	geom_point() +
	xlab('commit latency (ms)') +
	ylab('number placement groups primary') +
	ggtitle('OSD commit latency and pg number')

p19 <- ggplot(ceph_osd_stats_more, aes(x=apply_latency_ms, y=num_pgs)) +
	geom_point() +
	xlab('apply latency (ms)') +
	ylab('number placement groups primary') +
	ggtitle('OSD apply latency and pg number')

p20 <- ggplot(ceph_osd_stats_more, aes(x=commit_latency_ms, y=tot_object_copies)) +
	geom_point() +
	xlab('commit latency (ms)') +
	ylab('total object copies allotted to primary') +
	ggtitle('OSD commit latency and object copies')

p21 <- ggplot(ceph_osd_stats_more, aes(x=apply_latency_ms, y=tot_object_copies)) +
	geom_point() +
	xlab('apply latency (ms)') +
	ylab('total object copies allotted to primary') +
	ggtitle('OSD apply latency and object copies')


ggsave('p1-pdcceph3.png',p1)
ggsave('p2-pdcceph3.png',p2)
ggsave('p3-pdcceph3.png',p3)
ggsave('p4-pdcceph3.png',p4)
ggsave('p5-pdcceph3.png',p5)
ggsave('p6-pdcceph3.png',p6)
ggsave('p7-pdcceph3.png',p7)
ggsave('p9-pdcceph3.png',p9)
ggsave('p10-pdcceph3.png',p10)
ggsave('p11-pdcceph3.png',p11)
ggsave('p12-pdcceph3.png',p12)
ggsave('p13-pdcceph3.png',p13)
ggsave('p14-pdcceph3.png',p14)
ggsave('p15-pdcceph3.png',p15)
ggsave('p16-pdcceph3.png',p16)
ggsave('p17-pdcceph3.png',p17)
ggsave('p18-pdcceph3.png',p18)
ggsave('p19-pdcceph3.png',p19)
ggsave('p20-pdcceph3.png',p20)
ggsave('p21-pdcceph3.png',p21)

