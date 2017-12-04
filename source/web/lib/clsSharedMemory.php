<?php

/**
 * 共有メモリを利用するためのクラス
 * プロセス間のIN/OUTファイルの変わりに利用する想定でインタフェースを設計している。
 * 共有メモリとセマフォのライフサイクルは同じになる前提としている。
 */
class SharedMemory
{

	/**
	 * 共有メモリブロックのシステム ID
	 */
	protected $id;

	/**
	 * 共有メモリにアクセスするために使用するリソースID
	 */
	protected $shmid;

	/**
	 * セマフォにアクセスすするために使用するリソースID
	 */
	protected $sem_shmid;

	/**
	 * デフォルトのアクセス権限
	 */
	protected $perms = 0644;

	/**
	 * クラスを破棄するタイミングで共有メモリを削除するか否か
	 */
	protected $is_destroy_delete;

	/**
	 * 共有メモリ作成、またはオープン時にエラーが発生したか否か
	 */
	protected $is_error = false;

	/**
	 * 入力ファイルを表す変数キー
	 */
	protected $inputfile_key = 0;

	/**
	 * 出力ファイルを表す変数キー
	 */
	protected $outputfile_key = 1;

	/**
	 * コンストラクタ
	 */
	public function __construct()
	{
		// エラーログの記録先を指定
		ini_set('error_log', '/home/nisshin/public_html/source/log/e.log');
	}

	/**
	 * 共有メモリを作成する。
	 * 共有メモリにアクセスするプログラムのうち一番初めに実行されるプログラムが必ずこのメソッドを実行する必要あり。
	 *
	 * @param integer $id
	 *        	共有メモリブロックのシステム ID
	 * @param boolean $is_destroy_clear
	 *        	クラスを破棄するタイミングで共有メモリを削除するか否か
	 * @return 共有メモリの作成に成功した場合はtrue、失敗した場合はfalse
	 */
	public function create($id, $is_destroy_delete = true)
	{
		$this->id = $id;
		$this->is_destroy_delete = $is_destroy_delete;
		$this->is_error = false;

		// セマフォを取得する。
		$this->sem_shmid = sem_get($this->id, 1, $this->perms);
		if ($this->sem_shmid === false) {
			// セマフォの取得に失敗
			$this->is_error = true;
			return false;
		}

		try {
			sem_acquire($this->sem_shmid);

			// FIXME 定数化する必要あり 200KBメモリを確保
			$this->shmid = shm_attach($this->id, 1024 * 200, 0644);

			if ($this->shmid === false) {
				$this->is_error = true;
			}

			return $this->shmid;
		} finally {
			sem_release($this->sem_shmid);
		}
	}

	/**
	 * 作成済みの共有メモリをオープンする。
	 *
	 * @param integer $id
	 *        	共有メモリブロックのシステム ID
	 * @param boolean $is_destroy_clear
	 *        	クラスを破棄するタイミングで共有メモリを削除するか否か
	 * @return 共有メモリのオープンに成功した場合はtrue、失敗した場合はfalse
	 */
	public function open($id, $is_destroy_delete = false)
	{
		$this->id = $id;
		$this->is_destroy_delete = $is_destroy_delete;
		$this->is_error = false;

		// 共有メモリが作成されていない場合は処理失敗
		if (self::has_shared_memory($this->id) === false) {
			$this->is_error = true;
			return false;
		}

		// セマフォを取得する。
		$this->sem_shmid = sem_get($this->id);
		if ($this->sem_shmid === false) {
			// セマフォの取得に失敗
			$this->is_error = true;
			return false;
		}

		try {
			sem_acquire($this->sem_shmid);

			// 共有メモリは作成済みのはずなのでオプション指定なし
			$this->shmid = shm_attach($this->id);

			if ($this->shmid === false) {
				$this->is_error = true;
			}

			return $this->shmid;
		} finally {
			sem_release($this->sem_shmid);
		}
	}

	/**
	 * 共有メモリ作成、またはオープン時にエラーが発生したか否か返す。
	 *
	 * @return エラーが発生した場合はtrue、発生していない場合はfalse
	 */
	public function is_error()
	{
		return $this->is_error;
	}

	/**
	 * 入力ファイルにデータを書き込む。
	 *
	 * @param string $data
	 *        	データ
	 * @return 書き込みに成功した場合はtrue、書き込みに失敗した場合はfalse
	 */
	public function write_inputfile($data)
	{
		return $this->write($this->inputfile_key, $data);
	}

	/**
	 * 入力ファイルのデータを読み込む
	 *
	 * @param boolean $is_delete
	 *        	データを読み込んだ後にメモリのデータを削除するか否か
	 * @return データの書き込みがない場合はfalse、それ以外の場合は読み込んだデータ
	 */
	public function read_inputfile($is_delete = false)
	{
		return $this->read($this->inputfile_key, $is_delete);
	}

	/**
	 * 入力ファイルを削除する。
	 */
	public function delete_inputfile()
	{
		return $this->delete($this->inputfile_key);
	}

	/**
	 * 出力ファイルにデータを書き込む。
	 *
	 * @param string $data
	 *        	データ
	 * @return 書き込みに成功した場合はtrue、書き込みに失敗した場合はfalse
	 */
	public function write_outputfile($data)
	{
		return $this->write($this->outputfile_key, $data);
	}

	/**
	 * 出力ファイルのデータを読み込む
	 *
	 * @param boolean $is_delete
	 *        	データを読み込んだ後にメモリのデータを削除するか否か
	 * @return データの書き込みがない場合はfalse、それ以外の場合は読み込んだデータ
	 */
	public function read_outputfile($is_delete = false)
	{
		return $this->read($this->outputfile_key, $is_delete);
	}

	/**
	 * 出力ファイルを削除する。
	 */
	public function delete_outputfile()
	{
		return $this->delete($this->outputfile_key);
	}

	/**
	 * 共有メモリにデータを書き込む。
	 *
	 * @param integer $key
	 *        	キー
	 * @param string $data
	 *        	データ
	 * @return 書き込みに成功した場合はtrue、書き込みに失敗した場合はfalse
	 */
	private function write($key, $data)
	{
		try {
			sem_acquire($this->sem_shmid);

			return shm_put_var($this->shmid, $key, $data);
		} finally {
			sem_release($this->sem_shmid);
		}
	}

	/**
	 * 共有メモリからデータを読み込む。
	 *
	 * @param integer $key
	 *        	キー
	 * @param boolean $is_delete
	 *        	データを読み込んだ後にメモリのデータを削除するか否か
	 * @return データの書き込みがない場合はfalse、それ以外の場合は読み込んだデータ
	 */
	private function read($key, $is_delete = false)
	{
		try {
			sem_acquire($this->sem_shmid);

			// データなし
			if (shm_has_var($this->shmid, $key) === false) {
				return false;
			}

			$data = shm_get_var($this->shmid, $key);

			if ($is_delete) {
				// データ削除
				shm_remove_var($this->shmid, $key);
			}

			return $data;
		} finally {
			sem_release($this->sem_shmid);
		}
	}

	/**
	 * 共有メモリの領域を削除する。
	 *
	 * @param integer $key
	 *        	キー
	 */
	private function delete($key)
	{
		try {
			sem_acquire($this->sem_shmid);

			if (shm_has_var($this->shmid, $key) !== false) {
				// データ削除
				shm_remove_var($this->shmid, $key);
			}
		} finally {
			sem_release($this->sem_shmid);
		}
	}

	/**
	 * 共有メモリブロックのシステム IDを取得する。
	 *
	 * @return integer 共有メモリブロックのシステム ID
	 */
	public function getId()
	{
		return $this->id;
	}

	/**
	 * デストラクタ
	 */
	public function __destruct()
	{
		if ($this->sem_shmid !== false) {
			try {
				sem_acquire($this->sem_shmid);

				if ($this->shmid !== false) {
					// 共有メモリを削除して解放
					if ($this->is_destroy_delete) {
						shm_remove($this->shmid);
					}
					shm_detach($this->shmid);
				}
			} finally {
				// セマフォを解放して削除
				sem_release($this->sem_shmid);
				if ($this->is_destroy_delete) {
					sem_remove($this->sem_shmid);
				}
			}
		}
	}

	/**
	 * 共有メモリを作成済みか否かを判定する。
	 *
	 * @param integer $id
	 *        	共有メモリにアクセスするために使用するリソースID
	 * @return boolean 作成済みの場合はtrue、未作成の場合はfalse
	 */
	public static function has_shared_memory($id)
	{
		$key = sprintf("0x%'08x", $id);
		$output = shell_exec("ipcs -m | grep -i " . $key);
		$output = explode("\n", $output);

		if (! empty($output[0]) && $output[0] != "") {
			return true;
		}

		return false;
	}

	/**
	 * セマフォを作成済みか否かを判定する。
	 *
	 * @param integer $id
	 *        	セマフォにアクセスするために使用するリソースID
	 * @return boolean 作成済みの場合はtrue、未作成の場合はfalse
	 */
	public static function has_sem($id)
	{
		$key = sprintf("0x%'08x", $id);
		$output = shell_exec("ipcs -s | grep -i " . $key);
		$output = explode("\n", $output);

		if (! empty($output[0]) && $output[0] != "") {
			return true;
		}

		return false;
	}

	/**
	 * 共有メモリ、セマフォを破棄する。
	 *
	 * @param integer $id
	 *        	共有メモリ・セマフォにアクセスするために使用するリソースID
	 */
	public static function destroy($id)
	{
		if (self::has_shared_memory($id)) {
			$shmid = shm_attach($id);
			shm_remove($shmid);
			shm_detach($shmid);
		}
		if (self::has_sem($id)) {
			$sem_shmid = sem_get($this->id);
			sem_remove($sem_shmid);
		}
	}
}
?>
