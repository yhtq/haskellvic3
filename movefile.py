# 将指定目录（递归）下所有txt文件复制到指定目录
import os
import shutil
import sys
src_dir : str = sys.argv[1]
dst_dir : str = sys.argv[2]
if not os.path.exists(dst_dir):
    os.makedirs(dst_dir)
for root, dirs, files in os.walk(src_dir):
    for file in files:
        if file.endswith('.txt'):
            src_file = os.path.join(root, file)
            # 保持原有目录结构
            dst_file = os.path.join(dst_dir, src_file[len(src_dir):])
            if not os.path.exists(os.path.dirname(dst_file)):
                os.makedirs(os.path.dirname(dst_file))
            shutil.copyfile(src_file, dst_file)
print('copy files finished!')