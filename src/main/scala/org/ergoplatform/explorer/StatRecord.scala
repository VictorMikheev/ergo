package org.ergoplatform.explorer

case class StatRecord(timestamp: Long = 0L,
                      blockSize: Long = 0L,
                      totalSize: Long = 0L,
                      transactionCount: Long = 0L,
                      totalTransactionsCount: Long = 0L,
                      blocksCount: Long = 0L,
                      difficulty: Long = 0L,
                      blockCoins: Long = 0L,
                      totalCoins: Long = 0L,
                      blockValue: Long = 0L,
                      blockFee: Long = 0L,
                      totalMiningTime: Long = 0L,
                      blockMiningTime: Long = 0L,
                      version: String = "0.0.0",
                      height: Int = 0,
                      totalCoinsIssued: Long = 0L,
                      minerRevenue: Long = 0L)
