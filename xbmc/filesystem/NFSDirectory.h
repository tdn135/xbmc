#pragma once
/*
 *      Copyright (C) 2011-2012 Team XBMC
 *      http://www.xbmc.org
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XBMC; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#include "IDirectory.h"
#include "FileNFS.h"

namespace XFILE
{
  class CNFSDirectory : public IDirectory
  {
    public:
      CNFSDirectory(void);
      virtual ~CNFSDirectory(void);
      virtual bool GetDirectory(const CStdString& strPath, CFileItemList &items);
      virtual DIR_CACHE_TYPE GetCacheType(const CStdString &strPath) const { return DIR_CACHE_ONCE; };
      virtual bool Create(const char* strPath);
      virtual bool Exists(const char* strPath);
      virtual bool Remove(const char* strPath);
    private:
      bool GetServerList(CFileItemList &items);
      bool GetDirectoryFromExportList(const CStdString& strPath, CFileItemList &items);
      bool ResolveSymlink( const CStdString &dirName, struct nfsdirent *dirent, CURL &resolvedUrl);
  };
}

