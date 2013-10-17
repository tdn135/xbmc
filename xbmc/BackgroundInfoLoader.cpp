/*
 *      Copyright (C) 2005-2012 Team XBMC
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
 *  along with XBMC; see the file COPYING.  If not, see
 *  <http://www.gnu.org/licenses/>.
 *
 */

#include "BackgroundInfoLoader.h"
#include "FileItem.h"
#include "settings/AdvancedSettings.h"
#include "threads/SingleLock.h"
#include "utils/log.h"

using namespace std;

CBackgroundInfoLoader::CBackgroundInfoLoader() : m_thread (NULL)
{
  m_bStop = true;
  m_pObserver=NULL;
  m_pProgressCallback=NULL;
  m_pVecItems = NULL;
}

CBackgroundInfoLoader::~CBackgroundInfoLoader()
{
  StopThread();
}

void CBackgroundInfoLoader::Run()
{
  try
  {
    if (m_vecItems.size() > 0)
    {
      OnLoaderStart();

      while (!m_bStop)
      {
        CFileItemPtr pItem;
        vector<CFileItemPtr>::iterator iter = m_vecItems.begin();
        if (iter != m_vecItems.end())
        {
          pItem = *iter;
          m_vecItems.erase(iter);
        }

        if (pItem == NULL)
          break;

        // Ask the callback if we should abort
        if ((m_pProgressCallback && m_pProgressCallback->Abort()) || m_bStop)
          break;

        try
        {
          if (LoadItem(pItem.get()) && m_pObserver)
            m_pObserver->OnItemLoaded(pItem.get());
        }
        catch (...)
        {
          CLog::Log(LOGERROR, "%s::LoadItem - Unhandled exception for item %s", __FUNCTION__, pItem->GetPath().c_str());
        }
      }
    }

    OnLoaderFinish();
  }
  catch (...)
  {
    CLog::Log(LOGERROR, "%s - Unhandled exception", __FUNCTION__);
  }
}

void CBackgroundInfoLoader::Load(CFileItemList& items)
{
  StopThread();

  if (items.Size() == 0)
    return;

  CSingleLock lock(m_lock);

  for (int nItem=0; nItem < items.Size(); nItem++)
    m_vecItems.push_back(items[nItem]);

  m_pVecItems = &items;
  m_bStop = false;
  m_thread = new CThread(this, "BackgroundLoader");
  m_thread->Create();
#ifndef _LINUX
  m_thread->SetPriority(THREAD_PRIORITY_BELOW_NORMAL);
#endif
}

void CBackgroundInfoLoader::StopAsync()
{
  m_bStop = true;
}


void CBackgroundInfoLoader::StopThread()
{
  StopAsync();

  if (m_thread)
  {
    m_thread->StopThread();
    delete m_thread;
    m_thread = NULL;
  }
  m_vecItems.clear();
  m_pVecItems = NULL;
}

bool CBackgroundInfoLoader::IsLoading()
{
  return m_thread != NULL;
}

void CBackgroundInfoLoader::SetObserver(IBackgroundLoaderObserver* pObserver)
{
  m_pObserver = pObserver;
}

void CBackgroundInfoLoader::SetProgressCallback(IProgressCallback* pCallback)
{
  m_pProgressCallback = pCallback;
}

