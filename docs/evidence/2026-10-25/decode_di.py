import subprocess, re, struct
out = subprocess.run(["ioreg","-c","com_brightplaza_BrightPlazaTPer","-r","-d","1","-w0"],capture_output=True,text=True).stdout
m = re.search(r'"DTA Device Info" = <([0-9a-f]+)>', out)
b = bytes.fromhex(m.group(1))
print("blob length", len(b))
def bits(x): return [(x>>i)&1 for i in range(8)]
b2,b3,b4,b5 = bits(b[2]),bits(b[3]),bits(b[4]),bits(b[5])
names2 = ["TPer","Locking","Geometry","Enterprise","SingleUser","DataStore","OPAL20","OPAL10"]
names3 = ["Properties","ANY_OPAL_SSC","OPALITE","PYRITE","PYRITE2","RUBY","BlockSID","DataRemoval"]
names4 = ["NSLocking","FIPS","TPer_ACKNACK","TPer_async","TPer_bufferMgt","TPer_comIDMgt","TPer_streaming","TPer_sync"]
names5 = ["Locking_locked","Locking_lockingEnabled","Locking_lockingSupported","Locking_MBRshadowingNotSupported","Locking_MBRDone","Locking_MBREnabled","Locking_mediaEncrypt","Geometry_align"]
for ns,bb in [(names2,b2),(names3,b3),(names4,b4),(names5,b5)]:
    print({n:v for n,v in zip(ns,bb)})
off = 6
gran, = struct.unpack_from("<Q", b, off); off+=8
lbs, = struct.unpack_from("<I", b, off); off+=4
lal, = struct.unpack_from("<Q", b, off); off+=8
print("Geometry gran",gran,"lbs",lbs,"lowestAligned",lal)
off += 1  # Enterprise_rangeCrossing bitfield byte
ent_base, ent_n = struct.unpack_from("<HH", b, off); off+=4
off += 1  # SingleUser bits
su_lo, = struct.unpack_from("<I", b, off); off+=4
ds_max, ds_size, ds_align = struct.unpack_from("<HII", b, off); off+=10
o10_base,o10_n,o20_base,o20_n = struct.unpack_from("<HHHH", b, off); off+=8
o20_init,o20_rev,o20_ver,o20_minor = struct.unpack_from("<BBBB", b, off); off+=4
o20_admins,o20_users,o20_rc = struct.unpack_from("<HHB", b, off); off+=5
print("DataStore maxTables",ds_max,"maxSize",hex(ds_size),"align",ds_align)
print("OPAL20 base",hex(o20_base),"n",o20_n,"initialPIN",hex(o20_init),"revertedPIN",hex(o20_rev),"version",o20_ver,"minor",o20_minor,"admins",o20_admins,"users",o20_users,"rangeCrossing",o20_rc)
