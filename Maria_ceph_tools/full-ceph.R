library(jsonlite)

# Read in files
#ceph_crush <- fromJSON('root/ceph_json/ceph_crush.json')
#contains osds on their different hosts and the crush weights
#ceph_crush_rule_default <- fromJSON('root/ceph_json/ceph_crush_rule_default.json')
#no real data
#ceph_crush_tunables <- fromJSON('ceph_crush_tunables.json')
#no real data
ceph_osd_dump <- fromJSON('osd_dump.json')
#use to get pools and number of placement groups per pool settings
ceph_pg_dump <- fromJSON('pg_dump.json')
#use to get all the osd, pg stats

##########
# Clean up ceph_crush
#numhosts <- length(ceph_crush$items[[1]]$items)
#print (paste('number of hosts (4 before re-configure):',numhosts))
#for (i in (1:numhosts)){
#    ceph_crush$items[[1]]$items[[i]]$host <- ceph_crush$items[[1]]$name[i]
#}
#ceph_crush_stats <- do.call('rbind', ceph_crush$items[[1]]$items)
##limiting the stuff i care about to *stats
#numosds <- length(unique(ceph_crush_stats$name))
#print (paste('number of osds:',numosds))

##########
# Clean up osd dump data
timeosd <- ceph_osd_dump$modified
maxpool <- ceph_osd_dump$pool_max
maxosd <- ceph_osd_dump$max_osd

numpools <- length(ceph_osd_dump$pools$pool)
print (paste('number of pools:',numpools))

ceph_osd_pool_stats <- data.frame(ceph_osd_dump$pools$pool, ceph_osd_dump$pools$pool_name, ceph_osd_dump$pools$size, ceph_osd_dump$pools$pg_num, ceph_osd_dump$pools$pg_placement_num)
names(ceph_osd_pool_stats) <- c('pool','pool_name','size','pg_num','pg_placement_num')
#this contains all the pool/pg stats, see http://ceph.com/pgcalc/
numpgs <- sum(ceph_osd_pool_stats$pg_num)
print( paste('total number of placement groups:',numpgs))

##########
# Clean up pg dump data
timepg <- ceph_pg_dump$stamp
print(timepg)
pgfull_ratio <- ceph_pg_dump$full_ratio
pgnear_full_ratio <- ceph_pg_dump$near_full_ratio
# PG summary data
#note there's a lot of info in ceph_pg_dump$pg_stats_sum$stat_sum not pulled here
pgnum_bytes <- ceph_pg_dump$pg_stats_sum$stat_sum$num_bytes
pgnum_objects <- ceph_pg_dump$pg_stats_sum$stat_sum$num_objects
pgexp_copies <- ceph_osd_pool_stats$size[1] * pgnum_objects
pgnum_copies <- ceph_pg_dump$pg_stats_sum$stat_sum$num_object_copies
print(paste('total data size (TB):', pgnum_bytes/(10**12)))
print(paste('number of objects:', pgnum_objects))
print(paste('expected number of copies:', pgexp_copies))
print(paste('actual number of copies:', pgnum_copies))
print(paste('ave size of object (MB):', pgnum_bytes/(10**6)/pgnum_objects))

# OSD summary data
osd_raw_storageTB <- ceph_pg_dump$osd_stats_sum$kb/(10**9)
osd_raw_storagefullTB <- osd_raw_storageTB*pgfull_ratio
osd_raw_usedTB <- ceph_pg_dump$osd_stats_sum$kb_used/(10**9)
osd_raw_availTB <- ceph_pg_dump$osd_stats_sum$kb_avail/(10**9)
osd_usable_storageTB <- osd_raw_storageTB/ceph_osd_pool_stats$size[1]
osd_usable_storagefullTB <- osd_raw_storagefullTB/ceph_osd_pool_stats$size[1]
osd_usable_usedTB <- osd_raw_usedTB/ceph_osd_pool_stats$size[1]
osd_usable_availTB <- osd_raw_availTB/ceph_osd_pool_stats$size[1]

osd_commit_latency_ms <- ceph_pg_dump$osd_stats_sum$fs_perf_stat$commit_latency_ms
osd_apply_latency_ms <- ceph_pg_dump$osd_stats_sum$fs_perf_stat$apply_latency_ms
print(paste('OSD raw total storage (TB):', osd_raw_storageTB))
print(paste('at',pgfull_ratio,'max fill (what we use for raw stats), OSD raw storage (TB):', osd_raw_storagefullTB))
print(paste('OSD raw used storage (TB):', osd_raw_usedTB))
print(paste('OSD raw available storage (TB):', osd_raw_availTB))
print(paste('OSD usable total storage (TB):', osd_usable_storageTB))
print(paste('at',pgfull_ratio,'max fill (what we use for usable stats), OSD usable storage (TB):', osd_usable_storagefullTB))
print(paste('OSD usable used storage (TB):', osd_usable_usedTB))
print(paste('OSD usable available storage (TB):', osd_usable_availTB))
print(paste('OSD commit latency (ms):', osd_commit_latency_ms))
print(paste('OSD apply latency (ms):', osd_apply_latency_ms))

# PG stats
ceph_pg_dump$pg_stats$stat_sum$pgid <- ceph_pg_dump$pg_stats$pgid
ceph_pg_dump$pg_stats$stat_sum$up_primary <- ceph_pg_dump$pg_stats$up_primary
for (i in (1:numpgs)){
    ceph_pg_dump$pg_stats$stat_sum$up_osds[i] <- ceph_pg_dump$pg_stats$up[i]
}

ceph_pg_stats <- ceph_pg_dump$pg_stats$stat_sum

# Pool stats
ceph_pg_dump$pool_stats$stat_sum$poolid <- ceph_pg_dump$pool_stats$poolid
ceph_pg_dump$pool_stats$stat_sum$up <- ceph_pg_dump$pool_stats$up

ceph_pool_stats <- ceph_pg_dump$pool_stats$stat_sum
ceph_pool_stats <- merge(ceph_pool_stats, ceph_osd_pool_stats, by.x = 'poolid', by.y='pool')

# OSD stats
#not sure waht hb_in is, but also there if you want to look
ceph_osd_stats <- data.frame(ceph_pg_dump$osd_stats$osd, ceph_pg_dump$osd_stats$kb, ceph_pg_dump$osd_stats$kb_used, ceph_pg_dump$osd_stats$kb_avail, ceph_pg_dump$osd_stats$fs_perf_stat$commit_latency_ms, ceph_pg_dump$osd_stats$fs_perf_stat$apply_latency_ms)
names(ceph_osd_stats) <- c('osd','kb','kb_used','kb_available','commit_latency_ms','apply_latency_ms')
ceph_osd_stats$fill_fraction <- ceph_osd_stats$kb_used/ceph_osd_stats$kb 
print(paste('number of OSDs:',length(ceph_osd_stats$osd))) 
