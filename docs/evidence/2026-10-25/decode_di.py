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
# Namespace Locking 0x0403 (CNL 1.30 Table 6, PDF 51), added at the M3 review. The blob is sedutil a7eafd5's
# DTA_DEVICE_INFO (Common/DtaStructures.h:483-652, #pragma pack(1)); its NSLocking fields lie at 117-130, the
# device type at 139 and the serial at 143 anchoring the offsets. The kext copies them unswapped
# (TPerDriver.cpp:1399-1406 at macos-driver 8bf2635) from a descriptor struct one field off CNL's
# (DtaStructures.h:398-407: a reserved byte where CNL has Length, then three u32), so the fields it calls
# Max_Key_Count, Unused_Key_Count and Max_Range_Per_NS hold CNL's bytes 4-7, 8-11 and 12-15 as the drive sent
# them (big-endian); the descriptor's Length and its bytes 16-19 (Maximum Ranges Per Namespace) are not in the
# blob. Read 2026-10-30: version 1, byte 4 0x80 (Range_C 1), Maximum Key Count 9, Unused Key Count 8.
ns_version, ns_byte4 = b[117], b[119]
ns_max_keys, ns_unused_keys = struct.unpack_from(">II", b, 123)
print("NSLocking version", ns_version, "byte4", hex(ns_byte4), "Range_C", ns_byte4 >> 7 & 1, "Range_P", ns_byte4 >> 6 & 1,
      "SUM_C", ns_byte4 >> 5 & 1, "maxKeyCount", ns_max_keys, "unusedKeyCount", ns_unused_keys,
      "devType", struct.unpack_from("<I", b, 139)[0], "serial", b[143:163].decode("ascii", "replace").strip())
