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

#include "FileUPnP.h"
#include "UPnPDirectory.h"
#include "FileFactory.h"
#include "FileItem.h"
#include "utils/log.h"

using namespace XFILE;

CFileUPnP::CFileUPnP()
{
}

CFileUPnP::~CFileUPnP()
{
}

bool CFileUPnP::Open(const CURL& url)
{
  CFileItem item_new;
  if (CUPnPDirectory::GetResource(url.Get(), item_new))
  {
    //CLog::Log(LOGDEBUG,"FileUPnP - file redirect to %s.", item_new.GetPath().c_str());
    IFile *pNewImp = CFileFactory::CreateLoader(item_new.GetPath());    
    CURL *pNewUrl = new CURL(item_new.GetPath());    
    if (pNewImp)
    {
      throw new CRedirectException(pNewImp, pNewUrl);
    }
    SAFE_DELETE(pNewUrl);    
  }
  return false;
}

int CFileUPnP::Stat(const CURL& url, struct __stat64* buffer)
{
  CFileItem item_new;
  if (CUPnPDirectory::GetResource(url.Get(), item_new))
  {
    //CLog::Log(LOGDEBUG,"FileUPnP - file redirect to %s.", item_new.GetPath().c_str());
    IFile *pNewImp = CFileFactory::CreateLoader(item_new.GetPath());
    CURL *pNewUrl = new CURL(item_new.GetPath());
    if (pNewImp)
    {
      throw new CRedirectException(pNewImp, pNewUrl);
    }
    SAFE_DELETE(pNewUrl);
  }
  return -1;
}

bool CFileUPnP::Exists(const CURL& url)
{
  CFileItem item_new;
  if (CUPnPDirectory::GetResource(url.Get(), item_new))
  {
    //CLog::Log(LOGDEBUG,"FileUPnP - file redirect to %s.", item_new.GetPath().c_str());
    IFile *pNewImp = CFileFactory::CreateLoader(item_new.GetPath());
    CURL *pNewUrl = new CURL(item_new.GetPath());
    if (pNewImp)
    {
      throw new CRedirectException(pNewImp, pNewUrl);
    }
    SAFE_DELETE(pNewUrl);
  }
  return false;
}
